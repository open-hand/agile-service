import React, { useEffect, useState, useMemo } from 'react';
import { axios } from '@choerodon/boot';
import { IReportListBlock } from '@/routes/project-report/report-page/store';
import StatusTag from '@/components/StatusTag';
import { Issue } from '@/common/types';
import { getProjectId, getOrganizationId } from '@/utils/common';
import Table from './table';
import { flat2tree } from './utils';

interface Props {
  data: IReportListBlock
}
const ListBlock: React.FC<Props> = ({ data: { searchVO } }) => {
  const [data, setData] = useState([]);
  useEffect(() => {
    (async () => {
      const res = await axios({
        url: `/agile/v1/projects/${getProjectId()}/issues/include_sub`,
        method: 'post',
        params: {
          page: 1,
          size: 10,
          organizationId: getOrganizationId(),
        },
        data: searchVO,
        // data: {
        //   advancedSearchArgs: {},
        //   otherArgs: {
        //     customField: {
        //       option: [], date: [], date_hms: [], number: [], string: [], text: [],
        //     },
        //   },
        //   searchArgs: {},
        // },
      });
      setData(res.content);
    })();
  }, [searchVO]);
  const treeData = useMemo(() => flat2tree(data, { idKey: 'issueId' }), [data]);
  return (
    <div style={{ padding: '10px 26px' }}>
      <Table<Issue>
        data={treeData}
        primaryKey="issueId"
        columns={[{
          title: '概要',
          dataIndex: 'summary',
        }, {
          title: '编号',
          dataIndex: 'issueNum',
        }, {
          title: '经办人',
          dataIndex: 'assign',
          render: (item) => item.assigneeRealName,
        }, {
          title: '状态',
          dataIndex: 'status',
          render: (item) => (
            <StatusTag
              data={item.statusVO}
              style={{
                display: 'inline-block',
              }}
            />
          ),
        }, {
          title: '最后更新时间',
          dataIndex: 'lastUpdateDate',
        }]}
      />
    </div>
  );
};

export default ListBlock;
