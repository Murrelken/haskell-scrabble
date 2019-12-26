import {Component, OnInit} from '@angular/core';
import {HttpClient} from '@angular/common/http';

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css', './home.component.sass']
})
export class HomeComponent implements OnInit {

  fieldSize = 0;
  field = [];
  isNewGame = false;
  isGameSelected = false;
  isGameStarted = false;
  playerNumber = 0;
  gameNumber = 0;

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
      });
  }

  selectSize(size) {
    this.fieldSize = size;
    this.isNewGame = false;
    this.initializeField(size);
    this.createGame(size);
  }

  createGame(size) {
    console.log(size);
    this.http.post<any>(`api/createGame/${size}`, null)
      .subscribe(x => {
        this.gameNumber = x.gameNumber;
        this.playerNumber = x.givenPlayerNumber;
          console.log(x);
        }
      );
  }

  startListening() {
    let whileTrue = setInterval(() => {
      this.http.get<any>(`api/checkState/${this.gameNumber}`)
        .subscribe(x => {
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
      });
  }

  newGame() {
    this.isNewGame = true;
    this.isGameSelected = true;
  }
}
