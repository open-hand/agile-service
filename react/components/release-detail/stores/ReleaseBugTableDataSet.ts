import { versionApiConfig } from '@/api';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

function ReleaseStoryTableDataSet(versionId:string): DataSetProps {
  return {
    autoQuery: true,
    paging: false,
    selection: false,
    data: [{ summary: '测试问题测试问题测试问题测试问题测试问题测试问题测试问题测试问题测试问题', statusVO: {} }],
    fields: [
      { name: 'summary', label: '概要' },
      { name: 'issueNum', label: '编号' },
      { name: 'status', label: '状态' },
      { name: 'priority', label: '优先级' },
      { name: 'influenceVersion', label: '影响的版本' },

      { name: 'assigneeId', label: '经办人' },

    ],
    transport: {
      read: ({ params }) => ({ ...versionApiConfig.loadVersionBug(versionId, {}), params }),
    },
  };
}
export default ReleaseStoryTableDataSet;
