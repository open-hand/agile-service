import {
  observable, action, computed, runInAction,
} from 'mobx';
import { IReportContentType } from '@/common/types';

export interface IReportBlock {
  id: string
  title: string
  type: IReportContentType
  data: any
}

class ProjectReportStore {
  @observable blockList: IReportBlock[] = [{
    id: '1',
    title: '上周工作总结（文本标题）',
    type: 'text',
    data: '[{"insert":"描述内容\\n"}]',
  }, {
    id: '2',
    title: '上周未完成的工作项（列表标题）',
    type: 'list',
    data: {
      filter: {},
    },
  }, {
    id: '2',
    title: '上周未完成的工作项（图表标题）',
    type: 'chart',
    data: {
      chartType: 'burndown',
      filter: {},
    },
  }]
}

export default ProjectReportStore;
