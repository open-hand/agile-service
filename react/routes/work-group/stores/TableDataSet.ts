import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import DataSet from 'choerodon-ui/pro/lib/data-set';
import { workGroupApi, workGroupApiConfigApi } from '@/api';

interface TableProps {
  statusDs: DataSet,
  ROOT_ID: string,
  NOT_ASSIGN_ID: string,
}

export default ({
  statusDs,
  ROOT_ID,
  NOT_ASSIGN_ID,
}: TableProps): DataSetProps => ({
  autoCreate: false,
  autoQuery: true,
  cacheSelection: true,
  primaryKey: 'id',
  pageSize: 10,
  transport: {
    read: ({ data }) => {
      const {
        workGroupId = ROOT_ID, params, ...other
      } = data || {};
      const postData: { workGroupIds?: string[] } = { param: params, ...other || {} };
      switch (workGroupId) {
        case NOT_ASSIGN_ID:
          return workGroupApiConfigApi.loadUserUnAssignee(postData);
        case ROOT_ID:
          return workGroupApiConfigApi.loadUserUnAssigneeByGroup(postData);
        default:
          postData.workGroupIds = [workGroupId];
          return workGroupApiConfigApi.loadUserByGroup(postData);
      }
    },
  },
  fields: [{
    name: 'realName',
    type: FieldType.string,
    label: '用户名',
    bind: 'userVO.realName',
  }, {
    name: 'loginName',
    type: FieldType.string,
    label: '登录名',
    bind: 'userVO.loginName',
  }, {
    name: 'enabled',
    type: FieldType.boolean,
    label: '状态',
    bind: 'userVO.enabled',
  }, {
    name: 'ldap',
    type: FieldType.boolean,
    label: '认证来源',
    bind: 'userVO.ldap',
  }, {
    name: 'id',
    type: FieldType.string,
    bind: 'userVO.id',
  }, {
    name: 'workGroupName',
    type: FieldType.string,
    bind: 'workGroupVOS.name',
    label: '所属工作组',
  }],
  queryFields: [{
    name: 'realName',
    type: FieldType.string,
    label: '用户名',
  }, {
    name: 'loginName',
    type: FieldType.string,
    label: '登录名',
  }, {
    name: 'enabled',
    type: FieldType.string,
    label: '状态',
    options: statusDs,
    textField: 'text',
    valueField: 'value',
  }],
});
