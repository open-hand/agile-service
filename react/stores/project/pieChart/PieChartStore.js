/* eslint-disable no-return-assign */
/* eslint-disable no-param-reassign */
import {
  observable, action, computed, toJS,
} from 'mobx';
import _ from 'lodash';
import { reportApi } from '@/api';

const systemTypes = [
  { title: '经办人', value: 'assignee' },
  { title: '模块', value: 'component' },
  { title: '工作项类型', value: 'typeCode' },
  { title: '版本', value: 'version' },
  { title: '优先级', value: 'priority' },
  { title: '状态', value: 'status' },
  { title: '冲刺', value: 'sprint' },
  { title: '史诗', value: 'epic' },
  { title: '标签', value: 'label' },
  { title: '报告人', value: 'reporter' },
  { title: '主要负责人', value: 'mainResponsible' },
  { title: '环境', value: 'environment' },
  { title: '参与人', value: 'participant' },
];
class PieChartStore {
    @observable pieData = [];

    @observable sourceData=[];

    @observable colors = [];

    @observable pieLoading = false;

    @observable allTypes = [...systemTypes];

    @action setAllTypes = (types) => {
      this.allTypes = types;
    }

  @action changePieLoading(flag) {
      this.pieLoading = flag;
    }

  @action setPieData(data) {
    this.pieData = data;
  }

  @action setSourceData(data) {
    this.sourceData = data;
  }

  @action setColors(data) {
    this.colors = data;
  }

  @computed get getPieData() {
    return this.pieData;
  }

  @computed get getSourceData() {
    return this.sourceData;
  }

  @computed get getColors() {
    return this.colors;
  }

  getPieDatas = (type, sprintId, versionId, statusId) => {
    const currentType = this.allTypes.find((item) => item.value === type);
    if (currentType) {
      this.changePieLoading(true);
      reportApi.loadPie(currentType, sprintId, versionId, statusId)
        .then((data) => {
          const len = data.length;
          if (len) {
            const colors = ['#9665E2', '#F0657D', '#FAD352', '#FF9915', '#45A3FC', '#5365EA', '#47CBCA', '#59CB79', '#F953BA', '#D3D3D3'];
            if (len > 10) {
              for (let i = 10; i < len; i += 1) {
                // eslint-disable-next-line no-bitwise
                colors.push(`#${(`00000${((Math.random() * 16777215 + 0.5) >> 0).toString(16)}`).slice(-6)}`);
              }
            }
            this.setColors(colors);
            this.setSourceData(data);
            const bigData = data.filter((item) => item.percent >= 2);
            const otherData = {
              name: '其它', typeName: null, value: _.reduce(_.filter(data, (item) => item.percent < 2), (sum, item) => sum += item.value, 0), percent: _.reduce(_.filter(data, (item) => item.percent < 2), (sum, item) => sum += item.percent, 0).toFixed(2),
            };
            if (otherData.value > 0) {
              bigData.push(otherData);
            }
            this.setPieData(bigData);
          } else {
            this.setPieData([]);
          }
          this.changePieLoading(false);
        })
        .catch((error) => {
          this.changePieLoading(false);
        });
    }
  }
}

const pieChartStore = new PieChartStore();
export default pieChartStore;
