import React, {
  useState, useEffect, useRef, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Dropdown, Icon } from 'choerodon-ui/pro';
import { Issue } from '@/common/types';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { Placements } from 'choerodon-ui/pro/lib/dropdown/enum';
import IssueList from '../Component/IssueList';
import styles from './IssueSwitch.less';

function useClickOut(onClickOut: Function) {
  const ref = useRef();
  const handleClick = useCallback((e) => {
    // @ts-ignore
    if (ref.current && !ref.current.contains(e.target)) {
      onClickOut(e);
    }
  }, [onClickOut]);
  useEffect(() => {
    document.addEventListener('click', handleClick);
    return () => {
      document.removeEventListener('click', handleClick);
    };
  }, [handleClick]);
  return ref;
}

interface Props {
  issue: Issue,
  reloadIssue: Function,
}

const IssueSwitch: React.FC<Props> = ({ issue, reloadIssue }) => {
  const [visible, setVisible] = useState<boolean>(false);
  const {
    issueId, typeCode, relateIssueId, parentIssueId, sameParentBugVOList, sameParentIssueVOList,
  } = issue;
  const parentId = parentIssueId || relateIssueId;
  // const sameParentList = (sameParentIssueVOList || sameParentBugVOList || []).filter((item) => item.issueId !== issueId);
  const sameParentList = [
    {
      issueId: '91201683259543552',
      issueNum: 'AG-8588',
      typeCode: 'sub_task',
      statusId: '11',
      summary: '子任务1子任务1子任务1子任务1子任务1子任务1子任务1子任务1子任务1',
      reporterId: '10635',
      reporterName: '李文斐（20615）',
      description: null,
      assigneeId: '10635',
      assigneeName: '李文斐（20615）',
      projectId: 1528,
      epicId: 0,
      parentIssueId: '292489',
      storyPoints: null,
      versionIssueRelVOList: [],
      activeSprint: {
        sprintId: '7266', sprintName: 'IP-7:SPRINT-7266', startDate: null, endDate: null, statusCode: null, actualEndDate: null,
      },
      closeSprint: [],
      labelIssueRelVOList: [],
      componentIssueRelVOList: [],
      issueCommentVOList: [],
      issueAttachmentVOList: [],
      subIssueVOList: [],
      subBugVOList: [],
      objectVersionNumber: 2,
      creationDate: '2020-09-08 16:02:56',
      lastUpdateDate: '2020-09-09 10:57:37',
      estimateTime: null,
      remainingTime: null,
      epicName: null,
      issueEpicName: null,
      color: null,
      epicColor: null,
      sprintName: null,
      parentIssueNum: 'AG-8572',
      parentIssueSummary: '0.22开源版发布',
      assigneeImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      reporterImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      createrImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      createrName: '李文斐（20615）',
      createrLoginName: '20615',
      createrRealName: '李文斐',
      priorityId: '17',
      issueTypeId: '35',
      priorityVO: {
        id: '17', name: '中', description: '中', colour: '#3575DF', organizationId: '7', objectVersionNumber: 1, sequence: 1, enable: true, default: true,
      },
      issueTypeVO: {
        id: '35', name: '子任务', icon: 'agile_subtask', description: '子任务', organizationId: '7', colour: '#4d90fe', typeCode: 'sub_task', initialize: null, objectVersionNumber: 1, stateMachineName: null, stateMachineId: null,
      },
      createdBy: '10635',
      applyType: 'agile',
      createrEmail: 'wenfei.abcabcli@hand-china.abcabccom',
      assigneeLoginName: '20615',
      assigneeRealName: '李文斐',
      reporterLoginName: '20615',
      reporterRealName: '李文斐',
      relateIssueId: null,
      relateIssueNum: null,
      parentRelateSummary: null,
      featureVO: null,
      featureId: null,
      featureName: null,
      activePi: null,
      closePi: [],
      activePiTeams: null,
      activePiSprints: null,
      closedPiSprints: null,
      wsjf: {
        creationDate: null, createdBy: null, lastUpdateDate: null, lastUpdatedBy: null, objectVersionNumber: null, id: null, issueId: '91201683259543552', projectId: 1528, userBusinessValue: null, timeCriticality: null, rrOeValue: null, jobSize: null, costDelay: null, wsjf: null,
      },
      estimatedStartTime: null,
      estimatedEndTime: null,
      statusVO: {
        id: '11', name: '待处理', code: 'create', description: '待处理', type: 'todo', organizationId: '7', objectVersionNumber: 1, canDelete: null, completed: null, defaultStatus: null,
      },
    },
    {
      issueId: '91201816323837952',
      issueNum: 'AG-8589',
      typeCode: 'sub_task',
      statusId: '11',
      summary: '子任务2',
      reporterId: '10635',
      reporterName: '李文斐（20615）',
      description: null,
      assigneeId: null,
      assigneeName: null,
      projectId: 1528,
      epicId: 0,
      parentIssueId: '292489',
      storyPoints: null,
      versionIssueRelVOList: [],
      activeSprint: {
        sprintId: '7266', sprintName: 'IP-7:SPRINT-7266', startDate: null, endDate: null, statusCode: null, actualEndDate: null,
      },
      closeSprint: [],
      labelIssueRelVOList: [],
      componentIssueRelVOList: [],
      issueCommentVOList: [],
      issueAttachmentVOList: [],
      subIssueVOList: [],
      subBugVOList: [],
      objectVersionNumber: 1,
      creationDate: '2020-09-08 16:03:27',
      lastUpdateDate: '2020-09-08 16:03:27',
      estimateTime: null,
      remainingTime: null,
      epicName: null,
      issueEpicName: null,
      color: null,
      epicColor: null,
      sprintName: null,
      parentIssueNum: 'AG-8572',
      parentIssueSummary: '0.22开源版发布',
      assigneeImageUrl: null,
      reporterImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      createrImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      createrName: '李文斐（20615）',
      createrLoginName: '20615',
      createrRealName: '李文斐',
      priorityId: '17',
      issueTypeId: '35',
      priorityVO: {
        id: '17', name: '中', description: '中', colour: '#3575DF', organizationId: '7', objectVersionNumber: 1, sequence: 1, enable: true, default: true,
      },
      issueTypeVO: {
        id: '35', name: '子任务', icon: 'agile_subtask', description: '子任务', organizationId: '7', colour: '#4d90fe', typeCode: 'sub_task', initialize: null, objectVersionNumber: 1, stateMachineName: null, stateMachineId: null,
      },
      createdBy: '10635',
      applyType: 'agile',
      createrEmail: 'wenfei.abcabcli@hand-china.abcabccom',
      assigneeLoginName: null,
      assigneeRealName: null,
      reporterLoginName: '20615',
      reporterRealName: '李文斐',
      relateIssueId: null,
      relateIssueNum: null,
      parentRelateSummary: null,
      featureVO: null,
      featureId: null,
      featureName: null,
      activePi: null,
      closePi: [],
      activePiTeams: null,
      activePiSprints: null,
      closedPiSprints: null,
      wsjf: {
        creationDate: null, createdBy: null, lastUpdateDate: null, lastUpdatedBy: null, objectVersionNumber: null, id: null, issueId: '91201816323837952', projectId: 1528, userBusinessValue: null, timeCriticality: null, rrOeValue: null, jobSize: null, costDelay: null, wsjf: null,
      },
      estimatedStartTime: null,
      estimatedEndTime: null,
      statusVO: {
        id: '11', name: '待处理', code: 'create', description: '待处理', type: 'todo', organizationId: '7', objectVersionNumber: 1, canDelete: null, completed: null, defaultStatus: null,
      },
    },
    {
      issueId: '91201843494539264',
      issueNum: 'AG-8590',
      typeCode: 'sub_task',
      statusId: '11',
      summary: '子任务3',
      reporterId: '10635',
      reporterName: '李文斐（20615）',
      description: null,
      assigneeId: null,
      assigneeName: null,
      projectId: 1528,
      epicId: 0,
      parentIssueId: '292489',
      storyPoints: null,
      versionIssueRelVOList: [],
      activeSprint: {
        sprintId: '7266', sprintName: 'IP-7:SPRINT-7266', startDate: null, endDate: null, statusCode: null, actualEndDate: null,
      },
      closeSprint: [],
      labelIssueRelVOList: [],
      componentIssueRelVOList: [],
      issueCommentVOList: [],
      issueAttachmentVOList: [],
      subIssueVOList: [],
      subBugVOList: [],
      objectVersionNumber: 1,
      creationDate: '2020-09-08 16:03:34',
      lastUpdateDate: '2020-09-08 16:03:34',
      estimateTime: null,
      remainingTime: null,
      epicName: null,
      issueEpicName: null,
      color: null,
      epicColor: null,
      sprintName: null,
      parentIssueNum: 'AG-8572',
      parentIssueSummary: '0.22开源版发布',
      assigneeImageUrl: null,
      reporterImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      createrImageUrl: 'https://minio.choerodon.com.cn/iam-service/file_219ce90e5973404ca351485453de2c8d_A63D7C242F3DAC79F718E11F56595B8F.jpg',
      createrName: '李文斐（20615）',
      createrLoginName: '20615',
      createrRealName: '李文斐',
      priorityId: '17',
      issueTypeId: '35',
      priorityVO: {
        id: '17', name: '中', description: '中', colour: '#3575DF', organizationId: '7', objectVersionNumber: 1, sequence: 1, enable: true, default: true,
      },
      issueTypeVO: {
        id: '35', name: '子任务', icon: 'agile_subtask', description: '子任务', organizationId: '7', colour: '#4d90fe', typeCode: 'sub_task', initialize: null, objectVersionNumber: 1, stateMachineName: null, stateMachineId: null,
      },
      createdBy: '10635',
      applyType: 'agile',
      createrEmail: 'wenfei.abcabcli@hand-china.abcabccom',
      assigneeLoginName: null,
      assigneeRealName: null,
      reporterLoginName: '20615',
      reporterRealName: '李文斐',
      relateIssueId: null,
      relateIssueNum: null,
      parentRelateSummary: null,
      featureVO: null,
      featureId: null,
      featureName: null,
      activePi: null,
      closePi: [],
      activePiTeams: null,
      activePiSprints: null,
      closedPiSprints: null,
      wsjf: {
        creationDate: null, createdBy: null, lastUpdateDate: null, lastUpdatedBy: null, objectVersionNumber: null, id: null, issueId: '91201843494539264', projectId: 1528, userBusinessValue: null, timeCriticality: null, rrOeValue: null, jobSize: null, costDelay: null, wsjf: null,
      },
      estimatedStartTime: null,
      estimatedEndTime: null,
      statusVO: {
        id: '11', name: '待处理', code: 'create', description: '待处理', type: 'todo', organizationId: '7', objectVersionNumber: 1, canDelete: null, completed: null, defaultStatus: null,
      },
    },
  ];

  const renderIssueList = (item:Issue, i: number) => (
    <IssueList
      showAssignee={false}
      key={item.issueId}
      issue={{
        ...item,
        typeCode: item.typeCode || 'sub_task',
      }}
      i={i}
      onOpen={() => {
        if (reloadIssue) {
          reloadIssue(item.issueId);
        }
      }}
    />
  );

  const renderOverlay = () => (
    <div className={styles.issue_switch_overlay}>
      {
        // @ts-ignore
        sameParentList.map((item, i, arr) => renderIssueList(item, i === 0 ? arr.length : i))
      }
    </div>
  );

  const handleClickIcon = () => {
    setVisible(!visible);
  };

  useEffect(() => {
    setVisible(false);
  }, [issue.issueId]);

  const handleClickOut = useCallback(() => {
    setVisible(false);
  }, []);
  const ref = useClickOut(handleClickOut);

  return (
    <>
      {
        // @ts-ignore
        ['sub_task', 'bug'].includes(typeCode) && Boolean(parentId) && (
          // @ts-ignore
          <span className={styles.issue_switch} ref={ref}>
            <Icon
              type="arrow_drop_down"
              style={{
                cursor: 'pointer',
                marginTop: -5,
              }}
              onClick={handleClickIcon}
            />
            {visible && renderOverlay()}
            {/* <Dropdown
              trigger={['click'] as Action[]}
              overlay={renderOverlay()}
              placement={'bottomCenter' as Placements}
              align={{
                offset: [300, 300],
              }}
              getPopupContainer={(triggerNode) => {
                console.log(triggerNode);
                return document.getElementsByClassName(styles.issue_switch)[0];
              }}
            >
              <Icon
                type="arrow_drop_down"
                style={{
                  cursor: 'pointer',
                  marginTop: -5,
                }}
              />
            </Dropdown> */}
          </span>
        )
      }
    </>
  );
};

export default observer(IssueSwitch);
