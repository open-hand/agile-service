import React, { useContext, useState } from 'react';
import {
  Dropdown, Menu, Button, Modal,
} from 'choerodon-ui';
import { Permission } from '@choerodon/boot';
import { Modal as ModalPro } from 'choerodon-ui/pro';
import { issueApi } from '@/api';
import useHasDevops from '@/hooks/useHasDevops';
import useHasTest from '@/hooks/useHasTest';
import EditIssueContext from '../stores';
import Assignee from '../../Assignee';
import openIssueMove from './issue-move';

const { confirm } = Modal;
const IssueDropDown = ({
  onDeleteIssue, loginUserId, reloadIssue, testLinkStoreRef,
}) => {
  const {
    store, onUpdate, applyType,
  } = useContext(EditIssueContext);
  const docs = store.getDoc;
  const hasDevops = useHasDevops();
  const hasTest = useHasTest();

  const issue = store.getIssue;
  const {
    issueId, typeCode, createdBy, issueNum, subIssueVOList = [], assigneeId, objectVersionNumber, activePi, issueTypeVO, parentRelateSummary,
  } = issue;
  const disableFeatureDeleteWhilePiDoing = typeCode === 'feature' && activePi && activePi.statusCode === 'doing';
  const handleDeleteIssue = () => {
    const deleteModal = confirm({
      width: 560,
      title: `删除问题${issueNum}`,
      content:
        (
          <div>
            <p style={{ marginBottom: 10 }}>请确认您要删除这个问题。</p>
            <p style={{ marginBottom: 10 }}>该特性将会被彻底删除，包括所有附件、关联关系、评论。</p>
            <p style={{ marginBottom: 10 }}>如果您完成了这个问题，通常是已解决或者已关闭，而不是删除。</p>
            {
              subIssueVOList.length ? <p style={{ color: '#d50000' }}>{`注意：问题的${subIssueVOList.length}个子任务将被删除。`}</p> : null
            }
            {store.promptExtraNodeMap.has('delete.issue') ? store.promptExtraNodeMap.get('delete.issue')({ deleteModal: { destroy: () => deleteModal.destroy() } }) : null}
          </div>
        ),
      onOk() {
        return issueApi.delete(issueId, createdBy)
          .then((res) => {
            if (onDeleteIssue) {
              onDeleteIssue();
            }
          });
      },
      onCancel() { },
      okText: '删除',
      okType: 'danger',
    });
  };
  const handleClickMenu = (e) => {
    if (e.key === '0') {
      store.setWorkLogShow(true);
    } else if (e.key === 'item_1') {
      handleDeleteIssue(issueId);
    } else if (e.key === '2') {
      store.setCreateSubTaskShow(true);
    } else if (e.key === '3') {
      store.setCopyIssueShow(true);
    } else if (e.key === '4') {
      store.setTransformSubIssueShow(true);
    } else if (e.key === '5') {
      store.setTransformFromSubIssueShow(true);
    } else if (e.key === '6') {
      store.setCreateBranchShow(true);
    } else if (e.key === '7') {
      ModalPro.open({
        title: '分配问题',
        children: <Assignee
          issueId={issueId}
          assigneeId={assigneeId}
          objectVersionNumber={objectVersionNumber}
          onOk={() => {
            store.setAssigneeShow(false);
            if (onUpdate) {
              onUpdate();
            }
            reloadIssue(issueId);
          }}
          onCancel={() => {
            store.setAssigneeShow(false);
          }}
        />,
      });
    } else if (e.key === '8') {
      store.setChangeParentShow(true);
    } else if (e.key === '9') {
      store.setCreateSubBugShow(true);
    } else if (e.key === '10') {
      store.setRelateStoryShow(true);
    } else if (e.key === 'item_11') {
      openIssueMove({
        issue,
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
    <Menu onClick={handleClickMenu}>
      {!['feature', 'issue_epic'].includes(typeCode) && (
        <Menu.Item key="0">
          登记工作日志
        </Menu.Item>
      )}
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
      {['feature'].indexOf(typeCode) === -1 && (
        <Menu.Item key="3">
          复制问题
        </Menu.Item>
      )}
      {
        ['sub_task', 'feature', 'issue_epic'].indexOf(typeCode) === -1 && subIssueVOList.length === 0 && (
          <Menu.Item key="4">
            转化为子任务
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
      {
        applyType !== 'program' && hasDevops && (
          <Menu.Item key="6">
            创建分支
          </Menu.Item>
        )
      }
      {
        (typeCode !== 'feature' && typeCode !== 'issue_epic') && (
          <Menu.Item key="7">
            分配问题
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
        typeCode === 'bug' && !subIssueVOList.length > 0 && (
          <Menu.Item key="10">
            关联问题
          </Menu.Item>
        )
      }
      {
        (typeCode !== 'sub_task' && !parentRelateSummary) && ( // 子缺陷、子任务不能移
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
    </Menu>
  );
  return (
    <Dropdown
      overlay={getMenu()}
      trigger={['click']}
      getPopupContainer={(trigger) => trigger.parentNode}
      placement="bottomRight"
    >
      <Button icon="more_vert" />
    </Dropdown>
  );
};
export default IssueDropDown;
