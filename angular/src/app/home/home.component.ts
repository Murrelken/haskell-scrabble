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

  isSizeSelected = false;

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
  }

  addBook() {
    this.http.post<any>('api/post', {
      bookId: 1,
      bookName: 'Haruki...'
    })
      .subscribe();
  }

  getBooks() {
    this.http.get('api/get/1')
      .subscribe(x => {
        console.log(x);
      });
  }

  selectSize(size) {
    this.fieldSize = size;
    this.isSizeSelected = true;
    this.initializeField(size);
  }
}
