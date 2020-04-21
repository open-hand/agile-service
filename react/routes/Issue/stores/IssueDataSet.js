import React, { Fragment } from 'react';
import { set, runInAction } from 'mobx';
import { observer } from 'mobx-react-lite';
import { Button } from 'choerodon-ui';
import IssueStore from '@/stores/project/issue/IssueStore';
import Modal from '../components/Modal';
import transform from '../utils';
import BatchModal from '../components/BatchModal';

let modal;
function Header({ dataSet }) {
  return (
    <Fragment>
      <div style={{ fontSize: '30px', fontWeight: 500, marginRight: 12 }}>{dataSet.selected.length}</div>
      <div style={{ fontSize: '16px' }}>
        项已选中
      </div>
      <Button
        icon="close"
        shape="circle"
        style={{ color: 'white', marginLeft: 'auto' }}
        onClick={() => {
          modal.close();
        }}
      />
    </Fragment>
  );
}
const ObserverHeader = observer(Header);
function handleSelect({ dataSet }) {
  modal = Modal.open({
    key: 'modal',
    header: <ObserverHeader dataSet={dataSet} modal={modal} />,
    content: <BatchModal />,
  });
}
function handleUnSelect({ dataSet }) {
  if (dataSet.selected.length === 0) {
    modal.close();
  }
}
export default ({
  intl, projectId, organizationId, intlPrefix,
}) => ({
  primaryKey: 'issueId',
  autoQuery: false,
  selection: false,
  parentField: 'parentId',
  expandField: 'expand',
  idField: 'issueId',
  paging: true,
  cacheSelection: true,
  transport: {
    read: ({ dataSet }) => ({
      url: `/agile/v1/projects/${projectId}/issues/include_sub?organizationId=${organizationId}`,
      method: 'post',
      params: {
        page: dataSet.issueCurrentPage,
        size: dataSet.issuePageSize,
        sort: dataSet.sort && `${dataSet.sort},${dataSet.isAsc ? 'asc' : 'desc'}`,
      },
      transformRequest: (data) => {
        const searchDTO = IssueStore.getCustomFieldFilters();
        return JSON.stringify(searchDTO);
      },
      transformResponse: (res) => {
        const data = JSON.parse(res);
        // const data = test;
        runInAction(() => {
          set(dataSet, { issueTotal: data.total });
          if (data.pageSize > 0) {
            set(dataSet, { issuePageSize: data.pageSize });
          }
          set(dataSet, { issueCurrentPage: data.pageNum });
        });
        return data;
      },
    })
    ,
  },
  fields: [
    { name: 'issueId', type: 'number', label: '概要' },
    { name: 'issueTypeId', type: 'object', label: '问题类型' },
    { name: 'issueNum', type: 'string', label: '任务编号' },
    { name: 'priorityId', type: 'string', label: '优先级' },
    { name: 'statusId', type: 'object', label: '状态' },
    { name: 'assigneeId', type: 'string', label: '经办人' },
    { name: 'reporterId', type: 'string', label: '报告人' },
    { name: 'label', type: 'string', label: '标签' },
    { name: 'component', type: 'string', label: '模块' },
    { name: 'storyPoints', type: 'string', label: '故事点' },
    { name: 'version', type: 'string', label: '版本' },
    { name: 'epic', type: 'string', label: '史诗' },
    { name: 'feature', type: 'string', label: '特性' },
    { name: 'lastUpdateDate', type: 'string', label: '最后更新时间' },
    { name: 'issueSprintVOS', type: 'array', label: '冲刺' },
  ],
  queryFields: [
    { name: 'issueTypeId', type: 'array', label: '问题类型' },
    // { name: 'service', type: 'string', label: service },
    // { name: 'description', type: 'string', label: description },      
  ],
  events: {
    select: handleSelect,
    selectAll: handleSelect,
    unSelect: handleUnSelect,
    unSelectAll: handleUnSelect,
  },
});
