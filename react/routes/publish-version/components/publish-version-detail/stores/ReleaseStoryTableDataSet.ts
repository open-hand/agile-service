import { publishVersionApiConfig, versionApiConfig } from '@/api';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

function ReleaseStoryTableDataSet(versionId: string): DataSetProps {
  return {
    autoQuery: true,
    paging: true,
    selection: false,
    data: [{ summary: '测试问题测试问题测试问题测试问题测试问题测试问题测试问题测试问题测试问题' }],
    fields: [
      { name: 'summary', label: '概要' },
      { name: 'issueNum', label: '编号' },
      { name: 'status', label: '状态' },
      { name: 'appVersions', label: '应用版本' },
      { name: 'featureVO', label: '所属特性' },
      { name: 'epic', label: '所属史诗' },
      { name: 'assigneeId', label: '经办人' },

    ],
    transport: {
      read: ({ params }) => ({ ...publishVersionApiConfig.loadStory(versionId, {}, params) }),
    },
  };
}
export default ReleaseStoryTableDataSet;
