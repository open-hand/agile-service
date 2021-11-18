import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { workGroupApiConfigApi } from '@/api';

interface TableProps {
  workGroupId: string,
}

export default ({
  workGroupId,
}: TableProps): DataSetProps => ({
  autoCreate: false,
  autoQuery: true,
  autoQueryAfterSubmit: false,
  primaryKey: 'id',
  pageSize: 30,
  transport: {
    read: ({ data }) => {
      const {
        param,
      } = data || {};
      const postData = { workGroupIds: [workGroupId], param };
      return workGroupApiConfigApi.loadUserUnAssigneeByGroup(postData);
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
    name: 'id',
    type: FieldType.string,
    bind: 'userVO.id',
  }],
});
