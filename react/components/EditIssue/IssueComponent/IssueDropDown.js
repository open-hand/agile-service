import React, { useContext, useState, useRef } from 'react';
import {
  Button,
  Modal as ModalPro, Dropdown, Menu,
} from 'choerodon-ui/pro';
import { Permission } from '@choerodon/boot';

import { includes } from 'lodash';
import { toJS } from 'mobx';
import { issueApi } from '@/api';
import useHasDevops from '@/hooks/useHasDevops';
import useHasTest from '@/hooks/useHasTest';
import { openEditIssueCopyIssue } from '@/components/CopyIssue';
import { isInProgram } from '@/utils/program';
import openRecordWorkLogModal from '@/components/DailyLog/DailyLogPro';
import openCreateBranchModal from '@/components/CreateBranch/CreateBranchPro';
import EditIssueContext from '../stores';
import Assignee from '../../Assignee';
import openIssueMove from './issue-move';
import openChangeParentModal from './ChangeParent';
import openRelateIssueModal from './RelateIssue/RelateIssue';
import openTransformSubIssue from './TransformSubIssue/TransformSubIssue';
import openTransformFromSubIssue from './IssueBody/TransformFromSubIssue';

const IssueDropDown = ({
  onDeleteIssue, loginUserId, reloadIssue, testLinkStoreRef, onIssueCopy, onUpdate, onChangeParent, onRelateIssue, onTransformSubIssue, onOpenCreateSubTask, onOpenCreateSubBug,
}) => {
  const {
    store, applyType,
  } = useContext(EditIssueContext);
  const docs = store.getDoc;
  const hasDevops = useHasDevops();
  const hasTest = useHasTest();
  const [copyHasEpic, setCopyHasEpic] = useState(store.copyFields.find((item) => item.fieldCode === 'epic'));
  const dontCopyEpicRef = useRef();
  const handleCopyIssue = (issueId, isSubTask, dontCopyEpic, issue) => {
    store.setCopyIssueShow(false);
    reloadIssue(issue.issueId);
    if (onIssueCopy) {
      onIssueCopy(issue, issueId, isSubTask, dontCopyEpicRef.current);
    }
  };
  const issue = store.getIssue;
  const {
    issueId, typeCode, createdBy, issueNum, subIssueVOList = [], assigneeId, objectVersionNumber, activePi, issueTypeVO, parentRelateSummary, parentIssueId, relateIssueId,
  } = issue;
  const disableFeatureDeleteWhilePiDoing = typeCode === 'feature' && activePi && activePi.statusCode === 'doing';
  const handleDeleteIssue = () => {
    const deleteModal = ModalPro.open({
      width: 560,
      title: `删除工作项${issueNum}`,
      children:
        (
          <div>
            <p style={{ marginBottom: 10 }}>请确认您要删除这个工作项。</p>
            <p style={{ marginBottom: 10 }}>该工作项将会被彻底删除，包括所有附件、关联关系、评论。</p>
            <p style={{ marginBottom: 10 }}>如果您完成了这个工作项，通常是已解决或者已关闭，而不是删除。</p>
            {
              subIssueVOList.length ? <p style={{ color: '#d50000' }}>{`注意：工作项的${subIssueVOList.length}个子任务将被删除。`}</p> : null
            }
            {store.promptExtraNodeMap.has('delete.issue') ? store.promptExtraNodeMap.get('delete.issue')({ deleteModal: { destroy: () => deleteModal.close() } }) : null}
          </div>
        ),
      onOk() {
        return issueApi.project(store.projectId).delete(issueId, createdBy)
          .then((res) => {
            if (onDeleteIssue) {
              onDeleteIssue(issue);
            }
          });
      },
      okText: '删除',
    });
  };
  const dontCopyEpic = !!store.copyFields.find((item) => item.fieldCode === 'epic') && !copyHasEpic;
  dontCopyEpicRef.current = dontCopyEpic;

  const handleClickMenu = async (e) => {
    if (e.key === '0') {
      // store.setWorkLogShow(true);
      openRecordWorkLogModal({ issueId, projectId: store.projectId, onOk: () => reloadIssue(issueId) });
    } else if (e.key === 'item_11') {
      handleDeleteIssue();
    } else if (e.key === '2') {
      onOpenCreateSubTask();
    } else if (e.key === '3') {
      openEditIssueCopyIssue({
        projectId: store.projectId,
        issue,
        issueLink: store.getLinkIssues,
        issueSummary: issue.summary,
        // onCancel: () => store.setCopyIssueShow(false),
        onOk: handleCopyIssue.bind(this, parentIssueId || relateIssueId || issueId, typeCode === 'sub_task' || (typeCode === 'bug' && parentRelateSummary), dontCopyEpic),
        applyType,
        copyFields: store.copyFields,
        setCopyHasEpic,
      });
      store.setCopyIssueShow(true);
    } else if (e.key === '4') {
      openTransformSubIssue({
        issueId, objectVersionNumber, projectId: store.projectId, onOk: onTransformSubIssue,
      });
    } else if (e.key === '5') {
      openTransformFromSubIssue({
        issueId, objectVersionNumber, issueTypeId: issueTypeVO.id, projectId: store.projectId, onOk: onTransformSubIssue, store,
      });
    } else if (e.key === '6') {
      openCreateBranchModal({
        issueId, onOk: () => store.refreshBranch(), typeCode, defaultBranchSuffixName: issueNum, projectId: store.projectId,
      });
      store.setCreateBranchShow(true);
    } else if (e.key === '7') {
      ModalPro.open({
        title: '分配工作项',
        children: <Assignee
          issueId={issueId}
          projectId={store.projectId}
          assigneeId={assigneeId}
          objectVersionNumber={objectVersionNumber}
          onOk={(res) => {
            if (onUpdate) {
              onUpdate(res);
            }
            reloadIssue(issueId);
          }}
        />,
      });
    } else if (e.key === '8') {
      openChangeParentModal({
        issueId, issueNum, objectVersionNumber, onOk: onChangeParent, projectId: store.projectId,
      });
    } else if (e.key === '9') {
      onOpenCreateSubBug();
    } else if (e.key === '10') {
      openRelateIssueModal({ issue, onOk: onRelateIssue, projectId: store.projectId });
    } else if (e.key === 'item_10') {
      openIssueMove({
        issue,
        projectId: store.projectId,
        customFields: store.customFields,
        onMoveIssue: onDeleteIssue,
        loseItems: {
          test: hasTest && issueTypeVO.typeCode && ['feature', 'issue_epic'].indexOf(issueTypeVO.typeCode) === -1 && testLinkStoreRef.current?.data?.length,
          doc: docs && docs.knowledgeRelationList?.length,
          backlog: store.backlogLinks && store.backlogLinks.length,
          linkIssue: store.getLinkIssues && store.getLinkIssues.length,
        },
      });
    }
  };
  const getMenu = () => (
    <Menu onClick={handleClickMenu} selectable={false}>
      {!['feature', 'issue_epic'].includes(typeCode) && (
        <Menu.Item key="0">
          登记工作日志
        </Menu.Item>
      )}
      {
        ['sub_task', 'feature', 'issue_epic'].indexOf(typeCode) === -1 && !(typeCode === 'bug' && issue.relateIssueId) ? (
          <Menu.Item key="2">
            创建子任务
          </Menu.Item>
        ) : null
      }
      {
        ['story', 'task'].indexOf(typeCode) !== -1 && (
          <Menu.Item key="9">
            创建缺陷
          </Menu.Item>
        )
      }
      {
        (typeCode !== 'feature' && typeCode !== 'issue_epic') && (
          <Menu.Item key="7">
            分配工作项
          </Menu.Item>
        )
      }
      {
        typeCode === 'bug' && !subIssueVOList.length > 0 && (
          <Menu.Item key="10">
            关联工作项
          </Menu.Item>
        )
      }
      {
        applyType !== 'program' && hasDevops && (
          <Menu.Item key="6">
            创建分支
          </Menu.Item>
        )
      }
      {
        ['sub_task', 'feature', 'issue_epic'].indexOf(typeCode) === -1 && subIssueVOList.length === 0 && (
          <Menu.Item key="4">
            转化为子任务
          </Menu.Item>
        )
      }
      {
        typeCode === 'sub_task' && (
          <Menu.Item key="8">
            修改父级
          </Menu.Item>
        )
      }

      {
        typeCode === 'sub_task' && (
          <Menu.Item key="5">
            类型转换
          </Menu.Item>
        )
      }
      {['feature', ...(isInProgram() ? ['issue_epic'] : [])].indexOf(typeCode) === -1 && (
        <Menu.Item key="3">
          复制工作项
        </Menu.Item>
      )}
      {
        (includes(['story', 'task', 'bug'], typeCode) && !parentRelateSummary) && ( // 故事、任务、缺陷能移 子缺陷不能移
          <Permission
            service={['choerodon.code.project.cooperation.iteration-plan.ps.choerodon.code.agile.project.editissue.pro']}
            noAccessChildren={(
              <Menu.Item
                key="move"
                disabled={disableFeatureDeleteWhilePiDoing || (loginUserId && loginUserId.toString()) !== (createdBy && createdBy.toString())}
              >
                移动
              </Menu.Item>
            )}
          >
            <Menu.Item
              key="move"
            >
              移动
            </Menu.Item>
          </Permission>
        )
      }
      {
        <Permission
          service={['choerodon.code.project.cooperation.iteration-plan.ps.choerodon.code.agile.project.editissue.pro']}
          noAccessChildren={(
            <Menu.Item
              key="1"
              disabled={disableFeatureDeleteWhilePiDoing || (loginUserId && loginUserId.toString()) !== (createdBy && createdBy.toString())}
            >
              删除
            </Menu.Item>
          )}
        >
          <Menu.Item
            key="1"
          >
            删除
          </Menu.Item>
        </Permission>
      }

    </Menu>
  );
  return (
    <Dropdown
      overlay={getMenu()}
      trigger={['click']}
      placement="bottomRight"
    >
      <Button icon="more_vert" style={{ background: 'var(--primary-color)', color: 'white' }} />
    </Dropdown>
  );
};
export default IssueDropDown;
