/* eslint-disable no-return-assign */
/* eslint-disable no-param-reassign */
import {
  observable, action, computed, toJS,
} from 'mobx';
import _ from 'lodash';
import { reportApi } from '@/api';

class PieChartStore {
    @observable pieData = [];

    @observable sourceData=[];

    @observable colors = [];

    @observable pieLoading = false;

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
    this.changePieLoading(true);
    reportApi.loadPie(type, sprintId, versionId, statusId)
      .then((data) => {
        const len = data.length;
        if (len) {
          const colors = ['#9665E2', '#F0657D', '#FAD352', '#FF9915', '#45A3FC', '#3F51B5', '#47CBCA', '#59CB79', '#F953BA', '#D3D3D3'];
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

const pieChartStore = new PieChartStore();
export default pieChartStore;
