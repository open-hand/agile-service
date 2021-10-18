import React from 'react';
import { observer } from 'mobx-react-lite';
import { DatePicker } from 'choerodon-ui/pro';
import styles from './LogSearch.less';
import { useLogStore } from '../../stores';
import SelectUser from '@/components/select/select-user';
import { getIsOrganization } from '@/utils/common';
import SelectTeam from '@/components/select/select-team';

const LogSearch = () => {
  const {
    logSearchDs,
  } = useLogStore();

  return (
    <div className={styles.logSearch}>
      {
        getIsOrganization() && (
          <SelectTeam placeholder="所属项目" name="projectIds" dataSet={logSearchDs} />
        )
      }
      <DatePicker
        dataSet={logSearchDs}
        name="dateRange"
        placeholder={['开始时间', '结束时间']}
        style={{
          width: 260,
          marginLeft: 10,
        }}
      />
      <SelectUser
        dataSet={logSearchDs}
        name="userIds"
        placeholder="筛选成员"
        maxTagCount={2}
        maxTagTextLength={5}
        style={{
          marginLeft: 10,
        }}
      />
    </div>
  );
};

export default observer(LogSearch);
