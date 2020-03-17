import {
  observable, action, computed,
} from 'mobx';
import { stores } from '@choerodon/boot';
import { getProjectsInProgram, getProjectIsShowFeature } from '../../../api/CommonApi';

const { AppState } = stores;
/**
 * @param isInProgram 判断项目是否在项目群中
 * @param isShowFeature 判断项目是否显示特性 需先使用loadIsShowFeature 
 */
class IsInProgramStore {
  @observable isInProgram = false;

  @observable program = false;

  @observable isShowFeature = false; // 判断是否可以展示特性字段

  @observable artInfo = {};

  refresh = () => {
    if (AppState.currentMenuType.type === 'project') {
      getProjectsInProgram().then((program) => {
        // console.log(program);
        this.setIsInProgram(Boolean(program));
        this.setProgram(program);
      });
    }
  }

  loadIsShowFeature = () => {
    getProjectIsShowFeature().then((res) => {
      this.setIsShowFeature(Boolean(res));
      this.setArtInfo(res);
    });
  }

  @action setArtInfo(data) {
    this.artInfo = data;
  }

  @computed get getArtInfo() {
    return this.artInfo;
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

  @action setIsShowFeature(data) {
    this.isShowFeature = data;
  }

  @computed get getIsShowFeature() {
    return this.isShowFeature;
  }
}


export default new IsInProgramStore();
