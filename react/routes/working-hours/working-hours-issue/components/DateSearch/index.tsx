import React from 'react';
import { observer } from 'mobx-react-lite';
import { DatePicker, DataSet } from 'choerodon-ui/pro';
import styles from './DateSearch.less';

const DateSearch: React.FC<{ dateSearchDs: DataSet }> = ({ dateSearchDs }) => (
  <div className={styles.DateSearch}>
    <div className={styles.dateRange}>
      <DatePicker
        dataSet={dateSearchDs}
        name="startTime"
        placeholder="开始时间"
        style={{
          width: 120,
          marginRight: 5,
        }}
        clearButton={false}
      />
      <span>-</span>
      <DatePicker
        dataSet={dateSearchDs}
        name="endTime"
        placeholder="结束时间"
        style={{
          width: 120,
          marginLeft: 5,
        }}
        clearButton={false}
      />
    </div>
  </div>
);

export default observer(DateSearch);
