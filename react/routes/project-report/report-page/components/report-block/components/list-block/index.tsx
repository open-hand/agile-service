import React, {
  useEffect, useState, useMemo, useCallback, useRef,
} from 'react';
import { axios } from '@choerodon/boot';
import { find } from 'lodash';
import { IReportListBlock } from '@/routes/project-report/report-page/store';
import { Issue, IFoundationHeader } from '@/common/types';
import { getProjectId, getOrganizationId } from '@/utils/common';
import { fieldApi } from '@/api';
import UserHead from '@/components/UserHead';
import Table from './table';
import { flat2tree, getColumnByName } from './utils';

interface Props {
  data: IReportListBlock
}
const ListBlock: React.FC<Props> = ({ data: { searchVO, colList } }) => {
  const [data, setData] = useState([]);
  const [fields, setFields] = useState<IFoundationHeader[]>([]);
  const dataRef = useRef([]);

  const loadData = useCallback(async (page = 1) => {
    if (page === 1) {
      dataRef.current = [];
    }
    const res = await axios({
      url: `/agile/v1/projects/${getProjectId()}/issues/include_sub`,
      method: 'post',
      params: {
        page,
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
    dataRef.current = dataRef.current.concat(res.list);
    const hasNextPage = res.list.length > 0;
    if (hasNextPage) {
      loadData(page + 1);
    } else {
      setData(dataRef.current);
    }
  }, [searchVO]);
  const loadFields = useCallback(async () => {
    const Fields = await fieldApi.getFoundationHeader();
    setFields(Fields);
    loadData();
  }, [loadData]);
  useEffect(() => {
    loadFields();
  }, [loadFields]);
  const treeData = useMemo(() => flat2tree(data, { idKey: 'issueId' }), [data]);
  const columns = colList.map((name) => {
    const column = getColumnByName(name);
    if (column) {
      return column;
    }
    const field = find(fields, { code: name });
    return {
      title: field?.title || '',
      dataIndex: name,
      render: (issue: Issue) => {
        const { fieldType, code } = (field || {}) as IFoundationHeader;
        const value = issue.foundationFieldValue[code];
        if (fieldType === 'member') {
          return value && (
            <div style={{ display: 'inline-flex' }}>
              <UserHead
                // @ts-ignore
                user={value}
              />
            </div>
          );
        }
        return (
          <span>{value || ''}</span>
        );
      },
    };
  });
  return (
    <div style={{ padding: '10px 26px' }}>
      <Table<Issue>
        data={treeData}
        primaryKey="issueId"
        columns={columns}
      />
    </div>
  );
};

export default ListBlock;
