import React, { useMemo } from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { useDetail } from '@/components/detail-container';
import { versionApiConfig } from '@/api';
import ReleaseHome from './ReleaseHome';
import useFormatMessage from '@/hooks/useFormatMessage';

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
  const [detailProps] = useDetail();
  const formatMessage = useFormatMessage();
  const tableDataSet = useMemo(() => new DataSet({
    autoQuery: true,
    paging: true,
    selection: false,
    idField: 'versionId',
    fields: [
      { name: 'name', label: formatMessage({ id: 'agile.common.version' }) },
      { name: 'status', label: formatMessage({ id: 'agile.version.status' }) },
      { name: 'startDate', label: formatMessage({ id: 'agile.version.state.date' }) },
      { name: 'expectReleaseDate', label: formatMessage({ id: 'agile.version.expect.release.date' }) },
      { name: 'releaseDate', label: formatMessage({ id: 'agile.version.actual.release.date' }) },
      { name: 'description', label: formatMessage({ id: 'agile.common.description' }) },
      { name: 'programVersionInfoVOS', label: '关联项目群' },
    ],
    queryFields: [
      { name: 'name', label: formatMessage({ id: 'agile.common.version' }) },
      {
        name: 'status',
        label: formatMessage({ id: 'agile.version.status' }),
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
      { name: 'description', label: formatMessage({ id: 'agile.common.description' }) },

    ],
    transport: {
      read: ({ data, params }) => ({ ...versionApiConfig.loadVersionList(undefined, undefined, getSearchObj(data)), params }),
    },
  }), []);
  return <ReleaseHome tableDataSet={tableDataSet} detailProps={detailProps} />;
}
export default ReleaseHomeHoc;
