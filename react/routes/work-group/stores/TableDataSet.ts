import {DataSetProps} from 'choerodon-ui/pro/lib/data-set/DataSet';
import {FieldType} from 'choerodon-ui/pro/lib/data-set/enum';
import DataSet from 'choerodon-ui/pro/lib/data-set';
import {workGroupApi, workGroupApiConfigApi} from '@/api/WorkGroup';

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
        workGroupId = ROOT_ID, ...other
      } = data || {};
      const postData: { workGroupId?: string } = { ...other || {} };
      switch (workGroupId) {
        case NOT_ASSIGN_ID:
          return workGroupApiConfigApi.loadUserUnAssignee(postData);
        case ROOT_ID:
          return workGroupApiConfigApi.loadUserUnAssigneeByGroup(postData);
        default:
          postData.workGroupId = workGroupId;
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
