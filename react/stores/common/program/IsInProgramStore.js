import {
  observable, action, computed,
} from 'mobx';
import { stores } from '@choerodon/boot';
import { getProjectsInProgram } from '../../../api/CommonApi';

const { AppState } = stores;
class IsInProgramStore {
  @observable isInProgram = false;

  @observable program = false;


  refresh = () => {
    if (AppState.currentMenuType.type === 'project') {
      getProjectsInProgram().then((program) => {
        // console.log(program);
        this.setIsInProgram(Boolean(program));
        this.setProgram(program);
      });
    }
  }

  @action setIsInProgram(isInProgram) {
    this.isInProgram = isInProgram;
  }

  @action setProgram(program) {
    this.program = program;
  }

  @computed get getIsInProgram() {
    return this.isInProgram;
  }
}


export default new IsInProgramStore();
