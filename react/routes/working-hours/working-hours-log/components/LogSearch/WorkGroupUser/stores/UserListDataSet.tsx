import { DataSet } from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { DataSetSelection } from 'choerodon-ui/pro/lib/data-set/enum';
import { workGroupApiConfigApi } from '@/api/WorkGroup';
import { autoSelect } from '../utils';
import SelectUserStore from './SelectUserStore';

const handleSelect = ({ record }: { record: Record}, selectUserStore: SelectUserStore) => {
  const source = record.get('source');
  // 如果是自动选中的，不做处理
  if (source === 'auto') {
    record.set('source', undefined);
    return;
  }
  const caseId = record.get('caseId');
  const caseFolderId = record.get('folderId');
  // 选中树
  selectUserStore.handleCheckChange(true, caseFolderId);
  selectUserStore.addFolderSelectedCase(caseFolderId, caseId);
};
const handleUnSelect = ({ record }: { record: Record}, selectUserStore: SelectUserStore) => {
  const caseId = record.get('caseId');
  const caseFolderId = record.get('folderId');
  selectUserStore.removeFolderSelectedCase(caseFolderId, caseId);
};

const UserListDataSet = ({
  ROOT_ID,
  NOT_ASSIGN_ID,
  selectUserStore,
}: {
  ROOT_ID: string,
  NOT_ASSIGN_ID: string,
  selectUserStore: SelectUserStore
}): DataSetProps => ({
  primaryKey: 'caseId',
  autoQuery: false,
  selection: 'multiple' as DataSetSelection,
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
  events: {
    // 数据加载完后，自动选中
    load: ({ dataSet: ds }: { dataSet: DataSet}) => {
      autoSelect(ds, selectUserStore.treeMap);
    },
    select: handleSelect,
    unSelect: handleUnSelect,
    selectAll: ({ dataSet }: { dataSet: DataSet}) => {
      if (dataSet.length > 0) {
        // selectUserStore.handleCheckChange(true, folderId);
        dataSet.forEach((record) => handleSelect({ record }, selectUserStore));
      }
    },
    unSelectAll: ({ dataSet }: { dataSet: DataSet}) => {
      // selectUserStore.handleCheckChange(false, folderId);
      dataSet.forEach((record) => handleUnSelect({ record }, selectUserStore));
    },
  },
});

export default UserListDataSet;
