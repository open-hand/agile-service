import {
  observable, action, computed,
} from 'mobx';
import { stores } from '@choerodon/boot';
import Moment from 'moment';
import { extendMoment } from 'moment-range';
import { getCurrentPiInfo, getCurrentPiAllSprint } from '@/api/SprintApi.js';
import { getProjectsInProgram, getProjectIsShowFeature, getIsOwner } from '../../../api/CommonApi';

const moment = extendMoment(Moment);
const { AppState } = stores;
/**
 * @param isInProgram 判断项目是否在项目群中
 * @param isShowFeature 判断项目是否显示特性 需先使用loadIsShowFeature 
 */
class IsInProgramStore {
  @observable isOwner = true;

  @observable isInProgram = false;

  @observable program = false;

  @observable isShowFeature = false; // 判断是否可以展示特性字段

  @observable artInfo = {};

  @observable piInfo = {};

  @observable sprints = []; // 用于时间判断

  refresh = async () => {
    if (AppState.currentMenuType.type === 'project') {
      const program = await getProjectsInProgram();
      const hasProgram = Boolean(program);
      this.setIsInProgram(hasProgram);
      this.setProgram(program);
      if (hasProgram) {
        this.loadIsShowFeature();
      } else {
        this.setIsShowFeature(false);
      }
    }
  }

  getIsOwner = async () => {
    const isOwner = await getIsOwner();
    this.setIsOwner(isOwner);
  }

  loadIsShowFeature = async () => {
    const artInfo = await getProjectIsShowFeature();
    this.setIsShowFeature(Boolean(artInfo));
    this.setArtInfo(artInfo);
    return Boolean(artInfo);
  }

  loadPiInfoAndSprint = async (programId = this.artInfo.programId, artId = this.artInfo.id) => {
    const currentPiInfo = await getCurrentPiInfo(programId, artId);
    if (currentPiInfo.id) {
      const sprints = await getCurrentPiAllSprint(currentPiInfo.id);
      this.setPiInfo(currentPiInfo);
      this.setSprints(sprints.map(sprint => ({
        ...sprint,
        endDate: sprint.actualEndDate || sprint.endDate,
      })));
    }
  }

  @action setIsOwner(data) {
    this.isOwner = data;
  }

  @action setPiInfo(data) {
    this.piInfo = data;
  }

  @computed get getPiInfo() {
    return this.piInfo;
  }

  @action setSprints(data) {
    this.sprints = data;
  }

  @computed get getSprints() {
    return this.sprints.slice();
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

  // 时间点是否在pi内
  isDateBetweenPiDate(date) {
    const { actualStartDate: piActualStartDate, endDate: piEndDate } = this.getPiInfo;
    return date.isBetween(piActualStartDate, piEndDate);
  }

  // 时间点是否在其他冲刺中
  isDateBetweenOtherSprints(date, sprintId) {
    return this.sprints.filter(sprint => sprint.sprintId !== sprintId).some((sprint) => {
      const { startDate } = sprint;
      const endDate = sprint.actualEndDate || sprint.endDate;
      return date.isBetween(startDate, endDate);
    });
  }

  // 时间段是否在pi中
  isRangeInPi(startDate, endDate) {
    const { actualStartDate: piActualStartDate, endDate: piEndDate } = this.getPiInfo;
    const piRange = moment.range(piActualStartDate, piEndDate);
    // 开始时间和结束时间都在pi内
    return piRange.contains(startDate) && piRange.contains(endDate);
  }

  // 时间段是否和其他冲刺有重叠
  isRangeOverlapWithOtherSprints(startDate, endDate, sprintId) {
    return this.sprints.filter(sprint => sprint.sprintId !== sprintId).some((sprint) => {
      const { startDate: sprintStartDate } = sprint;
      const sprintEndDate = sprint.actualEndDate || sprint.endDate;
      const sprintRange = moment.range(sprintStartDate, sprintEndDate);
      const range = moment.range(startDate, endDate);
      return range.overlaps(sprintRange);
    });
  }


  // 开始时间应小于结束时间
  isRange(startDate, endDate) {
    return startDate < endDate;
  }

  // 时间能不能选
  dateCanChoose(date, sprintId) {
    // 首先时间应该在PI的实际开始时间和结束时间之间
    // 并且不能在其他冲刺之间
    return this.isDateBetweenPiDate(date) && !this.isDateBetweenOtherSprints(date, sprintId);
  }

  // 时间段是不是可以选
  rangeCanChoose(startDate, endDate, sprintId) {
    // 时间段要在pi内部
    // 时间段不能和其他冲刺重叠
    return this.isRange(startDate, endDate) && this.isRangeInPi(startDate, endDate) && !this.isRangeOverlapWithOtherSprints(startDate, endDate, sprintId);
  }
}


export default new IsInProgramStore();
