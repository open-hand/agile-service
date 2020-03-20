import {
  observable, action, computed,
} from 'mobx';
import { stores } from '@choerodon/boot';
import moment from 'moment';
import { getCurrentPiInfo, getCurrentPiAllSprint } from '@/api/SprintApi.js';
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

  @observable piInfo = {};

  @observable sprints = []; // 用于时间判断

  /**
   * 寻找冲刺中接近时间data最小范围
   * @param {*} data 
   * @param {*} sprintId 自身冲刺id
   */
  findDateMinRange(data, sprintId) {
    let minDate = null;
    const startDates = this.sprints.filter(sprint => sprint.sprintId !== sprintId && moment(sprint.startDate).isBefore(data)).map(item => item.endDate);
    const sprintDate = sprintId && this.sprints.find(sprint => sprint.sprintId === sprintId);

    if (startDates.length !== 0) {
      // eslint-disable-next-line prefer-destructuring
      minDate = startDates[0];
      startDates.forEach((startDate) => {
        if (sprintDate) {
          if (!moment(startDate).isBetween(sprintDate.startDate, sprintDate.endDate, null, '()') && moment(startDate).isAfter(moment(minDate))) {
            minDate = moment(startDate);
          }
        } else if (moment(startDate).isAfter(moment(minDate))) {
          minDate = moment(startDate);
        }
      });
    }
    return minDate;
  }


  /**
   * 寻找冲刺中接近时间data最大范围
   * @param {*} data 
   * @param {*} sprintId 自身冲刺id
   */
  findDateMaxRange(data, sprintId) {
    let maxDate = null;
    const startDates = this.sprints.filter(sprint => sprint.sprintId !== sprintId && moment(sprint.startDate).isAfter(data)).map(item => item.startDate);
    const sprintDate = sprintId && this.sprints.find(sprint => sprint.sprintId === sprintId);
    if (startDates.length !== 0) {
      // eslint-disable-next-line prefer-destructuring
      maxDate = startDates[0];
      startDates.forEach((startDate) => {
        if (sprintDate) {
          if (!moment(startDate).isBetween(sprintDate.startDate, sprintDate.endDate, null, '()') && moment(startDate).isBefore(moment(maxDate))) {
            maxDate = moment(startDate);
          }
        } else if (moment(startDate).isBefore(moment(maxDate))) {
          maxDate = moment(startDate);
        }
      });
    }
    return maxDate;
  }

  /**
   * 判断这个时间是否在冲刺时间范围内
   * @param {*} startDate 
   * @param {*} endDate 
   * @param {*} sprintId 自身冲刺id
   */
  stopChooseBetween(time, sprintId) {
    // const date = time.format('YYYY-MM-DD HH:mm:ss');
    const sprints = this.sprints.filter(sprint => sprint.sprintId !== sprintId);
    // eslint-disable-next-line no-plusplus
    for (let index = 0; index < sprints.length; index++) {
      const startDate = moment(sprints[index].startDate);
      const endDate = moment(sprints[index].endDate);
      const endDateZero = moment(sprints[index].endDate).hour(0).minute(0).second(0);
      if (moment(time).isBetween(startDate, endDateZero, null, '()')) {
        return true;
      } else if (moment(time).isBetween(endDateZero, endDate, null, '()')) {
        return true;
      }
    }

    return false;
  }

  refresh = () => {
    if (AppState.currentMenuType.type === 'project') {
      getProjectsInProgram().then((program) => {
        // console.log(program);
        this.setIsInProgram(Boolean(program));
        this.setProgram(program);
      });
    }
  }

  loadIsShowFeature = () => getProjectIsShowFeature().then((res) => {
    this.setIsShowFeature(Boolean(res));
    this.setArtInfo(res);
    return Boolean(res);
  })

  loadPiInfoAndSprint = (programId = this.artInfo.programId, artId = this.artInfo.id) => {
    getCurrentPiInfo(programId, artId).then((res) => {
      if (res.id) {
        getCurrentPiAllSprint(res.id).then((sprints) => {
          this.setPiInfo(res);
          this.setSprints(sprints);
        });
      }
    });
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
}


export default new IsInProgramStore();
