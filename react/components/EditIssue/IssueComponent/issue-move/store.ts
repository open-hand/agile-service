import { observable, action } from 'mobx';
import { IField } from '@/common/types';

class Store {
  @observable selfFields: IField[] = [];

  @action setSelfFields = (data: IField[]) => {
    this.selfFields = data;
  }

  @observable subTaskFields: IField[] = [];

  @action setSubTaskFields = (data: IField[]) => {
    this.selfFields = data;
  }

  @observable moveToProjectList = [];

  @action setMoveToProjectList = (data: any) => {
    this.moveToProjectList = data;
  }
}

const store = new Store();

export default store;
