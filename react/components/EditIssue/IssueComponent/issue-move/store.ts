import { observable, action } from 'mobx';
import { IField, User } from '@/common/types';

class Store {
  @observable selfFields: IField[] = [];

  @action setSelfFields = (data: IField[]) => {
    this.selfFields = data;
  }

  @observable subTaskFields: IField[] = [];

  @action setSubTaskFields = (data: IField[]) => {
    this.subTaskFields = data;
  }

  @observable moveToProjectList = [];

  @action setMoveToProjectList = (data: any) => {
    this.moveToProjectList = data;
  }

  subTaskDetailMap = observable.map();

  @observable subTaskTypeId: string | undefined;

  @action setSubTaskTypeId = (data?: string) => {
    this.subTaskTypeId = data;
  }

  @observable selectedUserIds: string[] = [];

  @action setSelectUserIds = (data: string[]) => {
    this.selectedUserIds = data;
  }

  @observable selectedUsers: User[] = [];

  @action setSelectedUsers = (data: User[]) => {
    this.selectedUsers = data;
  }
}

const store = new Store();

export default store;
