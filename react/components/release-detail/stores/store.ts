import { observable, action, computed } from 'mobx';
import { set, get, pick } from 'lodash';
import { IAppVersionData, projectApi, versionApi } from '@/api';
import IReleaseDetailData from '../types';

interface EventsProps {
  load: (data: any) => any | Promise<any>
  update: ((data: any) => any | Promise<any>)
}
interface INoticeMessage {
  type: 'cancel-feature' | 'select-feature',
  content?: string,
}
class ReleaseDetailStore {
  events: EventsProps = { update: () => true, load: () => true };

  @observable loading: boolean = false;

  @observable currentClickKDetail: IReleaseDetailData | undefined;

  @observable current: IReleaseDetailData | undefined;

  @observable appServiceList: Array<IAppVersionData & { name: string, type?: any }> = [];

  @observable visible: boolean = false;

  @observable disabled: boolean | undefined = false;

  @computed get getLoading() {
    return this.loading;
  }

  @computed get getVisible() {
    return this.visible;
  }

  @computed get getAppServiceList() {
    return this.appServiceList;
  }

  @action setAppServiceList(data: Array<any>) {
    this.appServiceList = data;
  }

  @action setDisabled(data: boolean) {
    this.disabled = data;
  }

  @action setCurrentData(data: IReleaseDetailData) {
    this.current = data;
  }

  @computed get getCurrentData() {
    return this.current || this.currentClickKDetail || {} as IReleaseDetailData;
  }

  @action clear() {
    this.loading = false;
    this.current = undefined;
    this.currentClickKDetail = undefined;
    this.visible = false;
    this.disabled = false;
    this.appServiceList = [];
  }

  @action init(initData?: { disabled?: boolean, events?: Partial<EventsProps>, programId?: string }) {
    this.clear();
    this.disabled = initData?.disabled;
    if (initData?.events) {
      Object.entries<any>(initData.events).forEach(([key, event]) => {
        set(this.events, key, event);
      });
    }
  }

  @action async loadData(id: string = this.getCurrentData.id, ignoreLoad: string[] = []) {
    this.loading = true;
    const versionData = ignoreLoad.includes('detail') ? this.current : await versionApi.load(id);

    const versionList = ignoreLoad.includes('app') ? this.getAppServiceList : await versionApi.loadAppVersionList(id);
    this.setAppServiceList(versionList.map((i: any) => ({ ...i, name: i.versionAlias || i.version })));

    this.setCurrentData({ ...versionData, id: versionData.versionId });
    await this.events.load({ versionData });
    this.loading = false;
  }

  @action('更新版本详情')
  async update(key: string, value: any) {
    this.loading = true;
    const data = pick(this.getCurrentData!, ['description', 'expectReleaseDate', 'name', 'objectVersionNumber', 'projectId', 'startDate']);
    set(data, key, value);
    await versionApi.update(this.getCurrentData.versionId, data);
    await this.loadData();
    await this.events.update(data);
  }

  @action select(data: string | IReleaseDetailData) {
    let waitQueryId = data;
    let newClickDetail = { id: data };
    if (typeof (data) === 'object') {
      waitQueryId = data.id;
      newClickDetail = data;
    }
    this.currentClickKDetail = newClickDetail as IReleaseDetailData;
    this.loadData(waitQueryId as string);
    this.visible = true;
  }

  @action unSelect() {
    this.visible = false;
    this.current = undefined;
    // this.current = {} as IPIAimData;
  }
}
const store = new ReleaseDetailStore();
export { ReleaseDetailStore };
export default store;
