import { publishVersionApiConfig, versionApiConfig } from '@/api';
import { set } from 'lodash';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { transformFilter } from '@/routes/Issue/stores/utils';

function IssueInfoTableDataSet(): DataSetProps {
  return {
    autoQuery: false,
    paging: true,
    selection: false,
    // data: [{ summary: '测试问题测试问题测试问题测试问题测试问题测试问题测试问题测试问题测试问题', statusVO: {} }],
    fields: [
      { name: 'summary', label: '概要' },
      { name: 'issueNum', label: '编号' },
      { name: 'status', label: '状态' },
      { name: 'priority', label: '优先级' },
      { name: 'influenceVersion', label: '影响的版本' },
      { name: 'tags', label: 'tag' },
      { name: 'creationDate', label: '创建时间' },

      { name: 'assigneeId', label: '经办人' },

    ],
    transport: {
      read: ({ params, data }) => {
        // console.log('params bug', data, params);
        const { search, versionId, issueTypeId } = data;
        console.log('params bug', data, params, issueTypeId);
        search && set(search, 'advancedSearchArgs.issueTypeId', [issueTypeId]);
        // {
        //   code: 'issueTypeId',
        //     name: '问题类型',
        //       defaultShow: true,
        //         fieldType: 'multiple',
        // }
        const filters = transformFilter([['issueTypeId', { value: [issueTypeId] }]]);

        return ({ ...publishVersionApiConfig.loadIssues(versionId, search || filters, params) });
      },
    },
  };
}
export default IssueInfoTableDataSet;
