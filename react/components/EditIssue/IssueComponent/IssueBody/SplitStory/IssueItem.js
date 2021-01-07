import React, { useCallback } from 'react';
import { useHistory } from 'react-router';
import { Tooltip } from 'choerodon-ui';
import ProjectHead from '@/components/ProjectHead';
import PriorityTag from '@/components/PriorityTag';
import StatusTag from '@/components/StatusTag';
import TypeTag from '@/components/TypeTag';
import styled from '@emotion/styled';
import { disableIssueEdit } from '@/utils/detail';
import { commonApi } from '@/api';
import { useDetailContainerContext } from '@/components/detail-container/context';

const Link = styled.a`
  overflow:hidden;
  text-overflow:ellipsis;
  white-space:nowrap;  
  cursor:pointer;
  color:#3f51b5;
`;
function IssueItem({ issue }) {
  const {
    issueId, issueTypeVO, issueNum, summary, priorityVO,
    statusVO, projectVO, totalCount, completedCount,
  } = issue;
  const { push } = useDetailContainerContext();
  const handleSummaryClick = async () => {
    const isViewProject = await commonApi.checkProjectViewPermission(projectVO.id);
    push({
      path: 'issue',
      props: {
        disabled: disableIssueEdit(projectVO.id),
        issueId,
        projectId: projectVO.id,
      },
    });
    // isViewProject && to(LINK_URL.workListIssue, {
    //   type: 'project',
    //   id: projectVO.id,
    //   params: {
    //     paramIssueId: issueId,
    //     paramName: issueNum,
    //   },
    // }, { blank: true });
  };

  return (
    <div
      style={{
        display: 'flex',
        alignItems: 'center',
        padding: '5px 0',
        cursor: 'pointer',
        borderBottom: '1px solid rgba(0, 0, 0, 0.12)',
      }}
    >
      <Tooltip mouseEnterDelay={0.5} title={`任务类型: ${issueTypeVO.name}`}>
        <TypeTag
          data={issue.issueTypeVO}
        />
      </Tooltip>
      <Tooltip title={`概要： ${issue.issueNum} ${issue.summary}`}>
        <div style={{
          margin: '0 4px', flex: 1, overflow: 'hidden', display: 'flex',
        }}
        >
          <Link style={{ margin: '0 2px', flexShrink: 0 }} onClick={handleSummaryClick}>{issueNum}</Link>
          <Link
            style={{ margin: '0 2px' }}
            onClick={handleSummaryClick}
          >
            {`${summary}`}
          </Link>
        </div>
      </Tooltip>
      <Tooltip title={`团队: ${projectVO.name}`}>
        <ProjectHead
          project={projectVO}
          hiddenText
          tooltip={false}
        />
      </Tooltip>
      {totalCount ? (
        <Tooltip
          mouseEnterDelay={0.5}
          title={(
            <div>
              {`全部issue：${totalCount}`}
              <br />
              {`已完成issue：${completedCount}`}
            </div>
          )}
        >
          <div style={{
            fontSize: '13px', fontWeight: 500, color: 'rgba(0,191,165,1)', margin: '0 4px',
          }}
          >
            {`${completedCount}/${totalCount}`}
          </div>
        </Tooltip>
      ) : null}
      <div style={{ margin: '0 4px', overflow: 'hidden' }}>
        <Tooltip mouseEnterDelay={0.5} title={`优先级： ${priorityVO.name}`}>
          <PriorityTag
            priority={priorityVO}
          />
        </Tooltip>
      </div>
      <div style={{
        margin: '0 4px', display: 'flex', justifyContent: 'flex-end',
      }}
      >
        <Tooltip mouseEnterDelay={0.5} title={`任务状态： ${statusVO.name}`}>
          <StatusTag
            data={statusVO}
          />
        </Tooltip>
      </div>
    </div>
  );
}

export default IssueItem;
