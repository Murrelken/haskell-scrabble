import {Component, OnInit} from '@angular/core';
import {HttpClient} from '@angular/common/http';

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css', './home.component.sass']
})
export class HomeComponent implements OnInit {

  fieldSize: number = 0;
  field = [];
  isNewGame = false;
  isGameSelected = false;
  isGameStarted = false;
  playerNumber = 0;
  gameNumber = 0;
  playerTurnNumber = 0;
  isYourTurn = false;
  isEnoughPlayers = false;
  isGameEnded = false;

  games = [];

  initializeField(size) {
    for (let i = 0; i < size; i++) {
      this.field.push([]);
      for (let ii = 0; ii <= i; ii++) {
        this.field[i].push('n');
      }
    }
  }

  constructor(private http: HttpClient) {
  }

  ngOnInit() {
    this.getAllGames();
  }

  getAllGames() {
    this.http.get<any>('api/getAll')
      .subscribe(x => {
        this.games = x;
        console.log(x);
      });
  }

  selectSize(size) {
    this.fieldSize = size;
    this.isNewGame = false;
    this.initializeField(size);
    this.createGame(size);
  }

  createGame(size) {
    this.http.post<any>(`api/createGame/${size}`, null)
      .subscribe(x => {
          this.gameNumber = x.gameNumber;
          this.playerNumber = x.givenPlayerNumber;
          this.startListening();
        }
      );
  }

  startListening() {
    let whileTrue = setInterval(() => {
      this.http.get<any>(`api/checkState/${this.gameNumber}`)
        .subscribe(x => {
          if (this.isGameStarted) {
            if (x.playerTurnNumber == this.playerNumber) {
              clearInterval(whileTrue);
              this.isYourTurn = true;
            }

            this.changeColor(x.changes.positionX, x.changes.positionY, `${x.changes.playerNumber}`);

            if (x.isGameEnded) {
              clearInterval(whileTrue);
              this.isGameEnded = x.isGameEnded;
            } else {
              this.playerTurnNumber = x.playerTurnNumber;
            }
          } else if (x.isGameStarted) {
            this.isGameStarted = true;
          }
          this.isEnoughPlayers = x.playersCount > 1;
        });
    }, 1000);
  }

  chooseGame(gameId) {
    this.isGameSelected = true;
    this.isNewGame = false;
    this.http.post<any>(`api/connectToGame/${gameId}`, null)
      .subscribe(x => {
        console.log(x);
        this.gameNumber = x.gameNumber;
        this.playerNumber = x.givenPlayerNumber;
        this.fieldSize = x.fieldSize;
        this.initializeField(this.fieldSize);
        this.startListening();
      });
  }

  newGame() {
    this.isNewGame = true;
    this.isGameSelected = true;
  }

  startGame() {
    this.http.post<any>(`api/startGame/${this.gameNumber}`, null)
      .subscribe(x => {
        this.isGameStarted = true;
      });
  }

  clickField(x: number, y: number) {
    if (this.isYourTurn && !this.isGameEnded) {
      this.http.post<any>(`api/makeTurn`, {
        turnChanges: {
          positionX: x,
          positionY: y,
          playerNumber: `${this.playerNumber}`
        },
        turnInfo: {
          gameNumber: this.gameNumber,
          givenPlayerNumber: this.playerNumber,
          fieldSize: 0
        }
      }).subscribe(_ => {
        this.changeColor(x, y, `${this.playerNumber}`);
        this.isYourTurn = false;
        this.http.get<any>(`api/checkIsGameEnded/${this.gameNumber}`)
          .subscribe(x => {
            this.isGameEnded = x.is;
            if (!this.isGameEnded) {
              this.startListening();
            }
          });
      }, err => {
        alert('You cn\'t make this turn');
      });
    }
  }

  changeColor(x: number, y: number, l) {
    this.field[x][y] = l;
  }
}
