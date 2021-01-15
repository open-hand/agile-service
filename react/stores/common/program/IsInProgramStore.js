import {
  observable, action, computed, runInAction,
} from 'mobx';
import { stores } from '@choerodon/boot';
import { commonApi } from '@/api';

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

  refresh = async () => {
    if (!(AppState.menuType.categories || []).map((c) => c.code).includes('N_PROGRAM') && AppState.currentMenuType?.type === 'project') {
      const program = await commonApi.getProjectsInProgram();
      const hasProgram = Boolean(program);
      this.setIsInProgram(hasProgram);
      this.setProgram(program);
      if (hasProgram) {
        this.loadIsShowFeature();
      } else {
        this.setIsShowFeature(false);
      }
    } else {
      this.setIsInProgram(false);
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

  @action setIsShowFeature(data) {
    this.isShowFeature = data;
  }

  @computed get getIsShowFeature() {
    return this.isShowFeature;
  }

  @action setArtInfo(data) {
    this.artInfo = data;
  }

  @computed get getArtInfo() {
    return this.artInfo;
  }

  loadIsShowFeature = async () => {
    const artInfo = await commonApi.getIsShowFeature();
    runInAction(() => {
      this.setArtInfo(artInfo);
      this.setIsShowFeature(Boolean(artInfo));
    });
    return Boolean(artInfo);
  }
}

export default new IsInProgramStore();
