import React, { useContext } from 'react';
import { filter, chunk, reverse } from 'lodash';
import { FormattedMessage } from 'react-intl';
import DataLogs from '../../Component/DataLogs';
import EditIssueContext from '../../stores';

const IssueLog = () => {
  const { store } = useContext(EditIssueContext);
  const renderDataLogs = () => {
    const stateDatalogs = store.getDataLogs;
    // 过滤掉影响的版本(bug)
    const datalogs = reverse(filter(stateDatalogs, (v) => v.field !== 'Version'));
    const newDataLogs = [];
    let autoTemp = [];
    datalogs.forEach((log, i, logs) => {
      if (log.field !== 'Auto Resolution' && log.field !== 'Auto Trigger' && log.field !== 'Auto Status') {
        newDataLogs.push(log);
      }
      if (log.field === 'Auto Status' || log.field === 'Auto Trigger' || log.field === 'Auto Resolution') {
        autoTemp.push(log);
        if (i + 1 === logs.length || (i + 1 < logs.length && ((logs[i + 1].field !== 'Auto Resolution' && logs[i + 1].field !== 'Auto Trigger' && logs[i + 1].field !== 'Auto Status') || logs[i + 1].field === 'Auto Status'))) {
          newDataLogs.push({
            ...autoTemp[0],
            logId: 'autoUpdate',
            field: 'autoUpdate',
            newStatus: autoTemp[0].newString,
            trigger: autoTemp[1].newString,
            resolutionChanged: autoTemp.length === 3,
            removeResolution: autoTemp.length === 3 && autoTemp[2].oldValue && !autoTemp[2].newValue,
          });
          autoTemp = [];
        }
      }
    });
    const issue = store.getIssue;
    const {
      typeCode, creationDate, createdBy,
      createrImageUrl, createrEmail,
      createrName, createrRealName, createrLoginName, issueTypeVO = {},
    } = issue;
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
      newString: issueTypeVO && issueTypeVO.name ? issueTypeVO.name : '',
      newValue: 'issueNum',
      logId: 'create',
    };
    return (
      <DataLogs
        datalogs={[...reverse(newDataLogs), createLog]}
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
          <FormattedMessage id="issue.data_log" />
        </div>
      </div>
      {renderDataLogs()}
    </div>
  );
};

export default IssueLog;
