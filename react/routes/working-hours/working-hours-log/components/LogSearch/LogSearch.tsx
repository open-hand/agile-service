import React from 'react';
import { observer } from 'mobx-react-lite';
import { DatePicker, DataSet, TreeSelect } from 'choerodon-ui/pro';
import { includes, map } from 'lodash';
import styles from './LogSearch.less';
import SelectUser from '@/components/select/select-user';
import { getIsOrganization } from '@/utils/common';
import SelectProject from '@/components/select/select-project';
import { workGroupApi } from '@/api';

const LogSearch: React.FC<{ searchDs: DataSet, showWorkGroup?: boolean }> = ({ searchDs, showWorkGroup }) => {
  const workGroupIds = searchDs.current?.get('workGroupIds');
  const userIds = searchDs.current?.get('userIds');
  return (
    <div className={styles.logSearch}>
      <div className={styles.dateRange}>
        <DatePicker
          dataSet={searchDs}
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
          dataSet={searchDs}
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
            dataSet={searchDs}
            multiple
            maxTagCount={2}
            maxTagTextLength={5}
          />
        )
      }
      {
        showWorkGroup && getIsOrganization() && (
          <TreeSelect
            dataSet={searchDs}
            name="workGroupIds"
            placeholder="筛选工作组"
            maxTagCount={2}
            maxTagTextLength={10}
            maxTagPlaceholder={(restValues) => `+${restValues.length}...`}
            searchable
            style={{
              marginLeft: 10,
            }}
          />
        )
      }
      <SelectUser
        dataSet={searchDs}
        name="userIds"
        key={`SLE-${workGroupIds?.length}`}
        placeholder="筛选成员"
        maxTagCount={2}
        maxTagTextLength={5}
        style={{
          marginLeft: 10,
        }}
        clearButton
        level={getIsOrganization() ? 'org' : 'project'}
        request={workGroupIds?.length ? ({ page, filter, requestArgs }: { page: number, filter: string, requestArgs: { selectedUserIds?: string[]}}) => workGroupApi.loadUserByGroupIds({ workGroupIds, realName: filter, userIds: requestArgs.selectedUserIds }, {
          page,
        }).then((res: any) => {
          const userList = res.content || [];
          const noFindUserIds: string[] = [];
          if (userIds.length) {
            userIds.forEach((id: string) => {
              if (!includes(map(userList, 'id'), id)) {
                noFindUserIds.push(id);
              }
            });
            searchDs.current?.set('userIds', userIds.filter((id: string) => !includes(noFindUserIds, id)));
          }
          return ({ ...res, content: userList, list: userList });
        }) : undefined}
      />
    </div>
  );
};

LogSearch.defaultProps = {
  showWorkGroup: false,
};
export default observer(LogSearch);
