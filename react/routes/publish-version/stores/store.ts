import {
  observable, action, computed, toJS,
} from 'mobx';
import {
  set, get, pick, merge, cloneDeep,
} from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import {
  devOpsApi,
  IAppVersionData, IPublishVersionData, IPublishVersionTreeNode, projectApi, publishVersionApi, versionApi,
} from '@/api';

interface EventsProps {
  load: (data: any) => any | Promise<any>
  createAfter: (data: any) => any /** 创建完发布版本事件 */
  delete: (data: any) => any
  selectIssue: (id: string) => any
  update: ((data: any, record: Record) => any | Promise<any>)
}

export interface IAppVersionDataItem extends IAppVersionData {
  name: string,
  type?: any,
  appService: boolean
  tag: boolean
  children?: IAppVersionDataItem[],
}
class PublishDetailStore {
  events: EventsProps = {
    update: () => true, load: () => true, selectIssue: () => { }, createAfter: () => { }, delete: () => { },
  };

  @observable loading: boolean = false;

  @observable currentClickKDetail: IPublishVersionData | undefined;

  @observable current: IPublishVersionData | undefined;

  @observable appServiceList: Array<{ id: string, name: string, code: string }> = [];
  // [{
  //   name: 'agile-test', appService: true, tag: false, artifactId: 'te:',
  // } as any];

  @observable currentMenu: 'detail' | 'diff' | 'info' = 'detail';

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

  findAppServiceByCode(code: string) {
    return this.getAppServiceList.find((service) => service.code === code);
  }

  @computed get getDependencyList() {
    return this.dependencyList;
  }

  @computed get getCurrentMenu() {
    return this.currentMenu;
  }

  @action setCurrentMenu(data: 'detail' | 'diff' | 'info') {
    this.currentMenu = data;
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

  @action init(initData?: { disabled?: boolean, events?: Partial<EventsProps> }) {
    // this.clear();
    this.disabled = initData?.disabled;
    if (initData?.events) {
      Object.entries<any>(initData.events).forEach(([key, event]) => {
        set(this.events, key, event);
      });
    }
  }

  @action async loadDependencyData(id: string = this.getCurrentData.id) {
    const dependencyList = await publishVersionApi.loadDependencyTree(id);
    this.setDependencyList(dependencyList[0]?.children || []);
  }

  @action async loadData(id: string = this.getCurrentData.id, ignoreLoad: string[] = []) {
    this.loading = true;
    const versionData: IPublishVersionData = ignoreLoad.includes('detail') ? this.current : await publishVersionApi.load(id);
    const appServiceList = await devOpsApi.loadActiveService();
    this.setAppServiceList(appServiceList);
    // const versionList = ignoreLoad.includes('app') ? this.getAppServiceList : await publishVersionApi.loadDependency(id);
    // this.setAppServiceList(versionList.map((i: any) => ({ ...i, name: `${i.artifactId}/${i.versionAlias || i.version}` })));
    this.loadDependencyData(id);
    this.setCurrentData({ ...versionData, name: versionData.versionAlias || versionData.version });
    await this.events.load({ versionData });
    this.loading = false;
  }

  @action('更新版本详情')
  async update(keyValues: { [key: string]: any } | string, record: Record, statusCode?: 'version_planning' | 'released') {
    this.loading = true;
    const data = cloneDeep(toJS(this.getCurrentData)) as any;
    // const data = pick(this.getCurrentData!, ['description', 'expectReleaseDate', 'name', 'objectVersionNumber', 'projectId', 'startDate']);

    Object.entries(keyValues).forEach(([key, v]) => {
      set(data, key, v);
    });

    const newData = await publishVersionApi.update(data.id, data, statusCode);
    data.id === this.getCurrentData.id && await this.loadData();
    await this.events.update(newData, record);
    this.loading = false;
  }

  @action('创建版本')
  async create(data: any) {
    this.loading = true;
    const res = await publishVersionApi.create(data);
    // this.select(res);
    await this.events.createAfter(res);
    this.loading = false;
  }

  @action('删除版本')
  async delete(publishVersionId: string) {
    this.loading = true;
    await publishVersionApi.delete(publishVersionId);
    await this.events.delete(publishVersionId);
    this.loading = false;
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
const store = new PublishDetailStore();
export { PublishDetailStore };
export default store;
