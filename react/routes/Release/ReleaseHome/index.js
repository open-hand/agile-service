import React, { useMemo } from 'react';
import { DataSet } from 'choerodon-ui/pro';
import useIsInProgram from '@/hooks/useIsInProgram';
import { useDetail } from '@/components/detail-container';
import { versionApiConfig } from '@/api';
import ReleaseHome from './ReleaseHome';

function getSearchObj(values = {}) {
  const searchArgs = {};
  if (values && values.name) {
    // eslint-disable-next-line prefer-destructuring
    searchArgs.name = values.name;
  }
  if (values && values.description) {
    // eslint-disable-next-line prefer-destructuring
    searchArgs.description = values.description;
  }
  return {
    advancedSearchArgs: {
      statusCodes: values
        && values.status && values.status.length > 0 ? values.status : [],
    },
    searchArgs,
    contents: values.params ? [values.params] : [],
  };
}
function ReleaseHomeHoc() {
  const { loading, ...restData } = useIsInProgram();
  const [detailProps] = useDetail();
  const tableDataSet = useMemo(() => new DataSet({
    autoQuery: true,
    paging: true,
    selection: false,
    idField: 'versionId',
    fields: [
      { name: 'name', label: '版本' },
      { name: 'status', label: '版本状态' },
      { name: 'startDate', label: '开始日期' },
      { name: 'expectReleaseDate', label: '预计发布日期' },
      { name: 'releaseDate', label: '实际发布日期' },
      { name: 'description', label: '描述' },
      { name: 'programVersionInfoVOS', label: '关联项目群' },
    ],
    queryFields: [
      { name: 'name', label: '版本' },
      {
        name: 'status',
        label: '版本状态',
        textField: 'text',
        valueField: 'value',
        multiple: true,
        options: new DataSet({
          data: [
            {
              text: '已归档',
              value: 'archived',
            },
            {
              text: '已发布',
              value: 'released',
            },
            {
              text: '规划中',
              value: 'version_planning',
            },
          ],
        }),
      },
      { name: 'description', label: '描述' },

    ],
    transport: {
      read: ({ data, params }) => ({ ...versionApiConfig.loadVersionList(undefined, undefined, getSearchObj(data)), params }),
    },
  }), []);
  return !loading && <ReleaseHome {...restData} tableDataSet={tableDataSet} detailProps={detailProps} />;
}
export default ReleaseHomeHoc;
