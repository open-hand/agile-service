import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { Pagination } from 'choerodon-ui/pro';
import Store from '../../stores';

function IssuePagination() {
  const { dataSet } = useContext(Store);
  return (
    <Pagination
      pageSize={dataSet.issuePageSize}
      page={dataSet.issueCurrentPage}
      total={dataSet.issueTotal}
      style={{ float: 'right', marginTop: 10 }}
      onChange={(page, pageSize) => {
        if (dataSet.issuePageSize !== pageSize) {
          dataSet.issuePageSize = pageSize;
          dataSet.issueCurrentPage = 1;
          dataSet.query();
        } else {
          dataSet.issueCurrentPage = page;
          dataSet.query();
        }
      }}
    />
  );
}
export default observer(IssuePagination);
