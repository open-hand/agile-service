import { observable, action, computed } from 'mobx';
import { set, get, pick } from 'lodash';
import {
  IAppVersionData, IPublishVersionData, IPublishVersionTreeNode, projectApi, publishVersionApi, versionApi,
} from '@/api';
import IReleaseDetailData from '../types';

interface EventsProps {
  load: (data: any) => any | Promise<any>
  selectIssue: (id: string) => any
  update: ((data: any) => any | Promise<any>)
}

export interface IAppVersionDataItem extends IAppVersionData {
  name: string,
  type?: any,
  appService: boolean
  tag: boolean
  children?: IAppVersionDataItem[],
}
class ReleaseDetailStore {
  events: EventsProps = { update: () => true, load: () => true, selectIssue: () => { } };

  @observable loading: boolean = false;

  @observable currentClickKDetail: IPublishVersionData | undefined;

  @observable current: IPublishVersionData | undefined;

  @observable appServiceList: Array<IAppVersionDataItem> = [{
    name: 'agile-test', appService: true, tag: false, artifactId: 'te:',
  } as any];

  @observable dependencyList: Array<IPublishVersionTreeNode> = [];

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

  @computed get getDependencyList() {
    return this.dependencyList;
  }

  @action setDependencyList(data: Array<any>) {
    this.dependencyList = data;
  }

  @action setAppServiceList(data: Array<any>) {
    this.appServiceList = data;
  }

  @action setDisabled(data: boolean) {
    this.disabled = data;
  }

  @action setCurrentData(data: IPublishVersionData) {
    this.current = data;
  }

  @computed get getCurrentData() {
    return this.current || this.currentClickKDetail || {} as IPublishVersionData;
  }

  @action clear() {
    this.loading = false;
    this.current = undefined;
    this.currentClickKDetail = undefined;
    this.visible = false;
    this.disabled = false;
    // this.appServiceList = [];
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

  @action async loadDependencyData(id: string = this.getCurrentData.id) {
    const dependencyList = await publishVersionApi.loadDependencyTree(id);
    this.setDependencyList(dependencyList);
  }

  @action async loadData(id: string = this.getCurrentData.id, ignoreLoad: string[] = ['app']) {
    this.loading = true;
    const versionData: IPublishVersionData = ignoreLoad.includes('detail') ? this.current : await publishVersionApi.load(id);

    const versionList = ignoreLoad.includes('app') ? this.getAppServiceList : await versionApi.loadAppVersionList(id);
    this.setAppServiceList(versionList.map((i: any) => ({ ...i, name: `${i.artifactId}/${i.versionAlias || i.version}` })));
    this.loadDependencyData(id);
    this.setCurrentData({ ...versionData, name: versionData.versionAlias || versionData.version });
    await this.events.load({ versionData });
    this.loading = false;
  }

  @action('更新版本详情')
  async update(key: string, value: any) {
    this.loading = true;
    const data = this.getCurrentData as any;
    // const data = pick(this.getCurrentData!, ['description', 'expectReleaseDate', 'name', 'objectVersionNumber', 'projectId', 'startDate']);
    set(data, key, value);
    await publishVersionApi.update(this.getCurrentData.id, data);
    await this.loadData();
    await this.events.update(data);
  }

  @action select(data: string | IPublishVersionData) {
    let waitQueryId = data;
    let newClickDetail = { id: data };
    if (typeof (data) === 'object') {
      waitQueryId = data.id;
      newClickDetail = data;
    }
    this.currentClickKDetail = newClickDetail as IPublishVersionData;
    this.loadData(waitQueryId as string);
    this.visible = true;
  }

  @action selectIssue(issueId: string) {
    this.events.selectIssue(issueId);
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
