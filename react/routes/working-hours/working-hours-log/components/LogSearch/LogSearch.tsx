import React from 'react';
import { observer } from 'mobx-react-lite';
import { DatePicker } from 'choerodon-ui/pro';
import styles from './LogSearch.less';
import { useLogStore } from '../../stores';
import SelectUser from '@/components/select/select-user';
import { getIsOrganization } from '@/utils/common';
import SelectProject from '@/components/select/select-project';

const LogSearch = () => {
  const {
    logSearchDs,
  } = useLogStore();

  return (
    <div className={styles.logSearch}>
      <div className={styles.dateRange}>
        <DatePicker
          dataSet={logSearchDs}
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
          dataSet={logSearchDs}
          name="endTime"
          placeholder="结束时间"
          style={{
            width: 120,
            marginLeft: 5,
          }}
          clearButton={false}
        />
      </div>
      {
        getIsOrganization() && (
          <SelectProject
            placeholder="所属项目"
            name="projectIds"
            dataSet={logSearchDs}
            multiple
            maxTagCount={2}
            maxTagTextLength={5}
          />
        )
      }
      <SelectUser
        dataSet={logSearchDs}
        name="userIds"
        placeholder="筛选成员"
        maxTagCount={2}
        maxTagTextLength={5}
        style={{
          marginLeft: 10,
        }}
        clearButton
      />
    </div>
  );
};

export default observer(LogSearch);
