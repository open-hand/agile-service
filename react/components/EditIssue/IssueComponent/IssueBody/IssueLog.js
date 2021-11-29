import React, { useContext } from 'react';
import {
  filter, pick,
} from 'lodash';
import { randomString } from '@/utils/random';
import DataLogs from '../../Component/DataLogs';
import EditIssueContext from '../../stores';

const IssueLog = () => {
  const { store } = useContext(EditIssueContext);
  const renderDataLogs = () => {
    const issue = store.getIssue;
    const {
      typeCode, creationDate, createdBy,
      createrImageUrl, createrEmail,
      createrName, createrRealName, createrLoginName, issueTypeVO = {},
    } = issue;
    const stateDatalogs = store.getDataLogs;
    // 过滤掉影响的版本(bug)
    const datalogs = filter(stateDatalogs, (v) => v.field !== 'Version');
    const newDataLogs = [];
    let autoTemp = [];
    let initialIssueType = issueTypeVO; // 最初的issue类型  在日志中无任务类型更改则是issueTypeVO
    datalogs.forEach((log, i, logs) => {
      if (log.field !== 'Auto Resolution' && log.field !== 'Auto Trigger' && log.field !== 'Auto Status') {
        newDataLogs.push(log);
      }
      if (log.field === 'Auto Status' || log.field === 'Auto Trigger' || log.field === 'Auto Resolution') {
        if (log.field === 'Auto Trigger' || log.field === 'Auto Resolution') {
          autoTemp.push(log);
        } else if (log.field === 'Auto Status') {
          if (autoTemp.length > 0) {
            autoTemp.push(log);
            autoTemp = autoTemp.reverse();
            newDataLogs.push({
              ...autoTemp[0],
              ...pick(autoTemp[1], ['lastUpdateDate', 'creationDate']),
              logId: `autoUpdate-${randomString(10)}`, // 加入随机数 避免更改详情，增加日志时重复
              field: 'autoUpdate',
              newStatus: autoTemp[0].newString,
              trigger: autoTemp[1].newString,
              resolutionChanged: autoTemp.length === 3,
              removeResolution: autoTemp.length === 3 && autoTemp[2].oldValue && !autoTemp[2].newValue,
            });
          }
          autoTemp = [];
        }
      }
      if (!initialIssueType.isInitial && log.field === 'issuetype') {
        initialIssueType = { name: log.oldString, isInitial: true };
      }
    });

    // 创建Issue日志
    const createLog = {
      email: createrEmail,
      field: 'createInitType',
      imageUrl: createrImageUrl,
      name: createrName,
      realName: createrRealName,
      loginName: createrLoginName,
      lastUpdateDate: creationDate,
      lastUpdatedBy: createdBy,
      newString: initialIssueType && initialIssueType.name ? initialIssueType.name : '',
      newValue: 'issueNum',
      logId: 'create',
    };
    return (
      <DataLogs
        datalogs={[...newDataLogs, createLog]}
        typeCode={typeCode}
        createdById={createdBy}
        creationDate={creationDate}
      />
    );
  };

  return (
    <div id="data_log">
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          操作历史
        </div>
      </div>
      {renderDataLogs()}
    </div>
  );
};

export default IssueLog;
