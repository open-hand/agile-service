import {
  observable, action, computed,
} from 'mobx';
import _ from 'lodash';
import { reportApi } from '@/api';

const UNIT_STATUS = {
  issue_count: {
    committed: 'committedIssueCount',
    completed: 'completedIssueCount',
  },
  story_point: {
    committed: 'committedStoryPoints',
    completed: 'completedStoryPoints',
  },
  remain_time: {
    committed: 'committedRemainTime',
    completed: 'completedRemainTime',
  },
};
const UNIT2NAME = {
  story_point: '故事点',
  issue_count: '工作项计数',
  remain_time: '剩余时间',
};

class VelocityChartStore {
  @observable tableLoading = false;

  @observable tableData = [];

  @observable chartLoading = false;

  @observable chartData = [];

  @observable beforeCurrentUnit = 'story_point';

  @observable currentUnit = 'story_point';

  loadChartAndTableData() {
    this.loadChartData();
  }

  loadChartData(unit = this.currentUnit) {
    this.setChartLoading(true);
    this.setTableLoading(true);
    reportApi.loadVelocity(unit).then((res) => {
      this.setBeforeCurrentUnit(unit);
      this.setChartData(res);
      this.setChartLoading(false);
      this.setTableLoading(false);
    });
  }

  @action setTableLoading(data) {
    this.tableLoading = data;
  }

  @action setTableData(data) {
    this.tableData = data;
  }

  @action setChartLoading(data) {
    this.chartLoading = data;
  }

  @action setChartData(data) {
    this.chartData = data;
  }

  @action setBeforeCurrentUnit(data) {
    this.beforeCurrentUnit = data;
  }

  @action setCurrentUnit(data) {
    this.currentUnit = data;
  }

  @computed get getChartDataX() {
    const sprints = _.map(this.chartData, 'sprintName');
    return sprints;
  }

  @computed get getChartDataYCommitted() {
    const prop = UNIT_STATUS[this.beforeCurrentUnit].committed;
    const committed = _.map(this.chartData, prop);
    return committed;
  }

  @computed get getChartDataYCompleted() {
    const prop = UNIT_STATUS[this.beforeCurrentUnit].completed;
    const completed = _.map(this.chartData, prop);
    return completed;
  }

  @computed get getChartYAxisName() {
    const name = UNIT2NAME[this.beforeCurrentUnit];
    return name;
  }
}

const velocityChartStore = new VelocityChartStore();
export default velocityChartStore;
