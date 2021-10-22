import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { Dropdown, Menu, Icon } from 'choerodon-ui/pro';
import { find } from 'lodash';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import { issueApi } from '@/api';
import useIsInProgram from '@/hooks/useIsInProgram';
import TypeTag from '../../TypeTag';
import EditIssueContext from '../stores';
import './IssueComponent.less';
import openRequiredFieldsModal from './required-fields';

const IssueType = observer(({
  reloadIssue, applyType, onTransformType,
}) => {
  const { store, disabled } = useContext(EditIssueContext);
  let { data: issueTypeData } = useProjectIssueTypes({ onlyEnabled: true, applyType, projectId: store.projectId }, { enabled: !disabled });
  const handleChangeType = async (type) => {
    const issue = store.getIssue;
    const {
      issueId, objectVersionNumber, summary, featureVO = {}, issueTypeVO = {},
    } = issue;
    const { featureType, value } = type.item.props;
    const { typeCode } = issueTypeVO;
    if (typeCode === 'feature') {
      const { id, objectVersionNumber: featureObjNum } = featureVO;
      const issueUpdateVO = {
        issueId,
        objectVersionNumber,
        featureVO: {
          id,
          issueId,
          objectVersionNumber: featureObjNum,
          featureType: type.item.props.value,
        },
      };
      issueApi.project(store.projectId).update(issueUpdateVO)
        .then((newIssue) => {
          if (reloadIssue) {
            reloadIssue(issueId);
          }
          if (onTransformType) {
            onTransformType(newIssue, issue);
          }
        });
    } else {
      const res = await issueApi.project(store.projectId).getRequiredField(issueId, value);
      if (res && res.length) {
        openRequiredFieldsModal({
          projectId: store.projectId,
          requiredFields: res,
          issueVO: {
            summary,
            issueId,
            issueTypeVO,
            objectVersionNumber,
            typeCode: type.item.props.typeCode,
            issueTypeId: value,
          },
          reloadIssue,
          onUpdate: (newIssue) => {
            if (reloadIssue) {
              reloadIssue(issueId);
            }
            if (onTransformType) {
              onTransformType(newIssue, issue);
            }
          },
        });
      } else {
        const issueUpdateTypeVO = {
          epicName: type.item.props.typeCode === 'issue_epic' ? summary : undefined,
          issueId,
          objectVersionNumber,
          typeCode: type.item.props.typeCode,
          issueTypeId: value,
          featureType,
        };
        issueApi.project(store.projectId).updateType(issueUpdateTypeVO)
          .then((newIssue) => {
            if (reloadIssue) {
              reloadIssue(issueId);
            }
            if (onTransformType) {
              onTransformType(newIssue, issue);
            }
          });
      }
    }
  };
  const issue = store.getIssue;
  const { isInProgram } = useIsInProgram({ projectId: store.projectId });
  const { issueTypeVO = {}, featureVO = {}, subIssueVOList = [] } = issue;
  const { typeCode, id } = issueTypeVO;
  const { stateMachineId } = find(issueTypeData, { id }) || {};
  const { featureType } = featureVO || {};
  let currentIssueType = issueTypeVO;
  if (typeCode === 'feature') {
    issueTypeData = [
      {
        ...issueTypeVO,
        colour: '#3D5AFE',
        featureType: 'business',
        name: '特性',
        id: 'business',
      }, {
        ...issueTypeVO,
        colour: '#FFCA28',
        featureType: 'enabler',
        name: '使能',
        id: 'enabler',
      },
    ];
    currentIssueType = featureType === 'business' ? issueTypeData[0] : issueTypeData[1];
  } else if (typeCode === 'sub_task') {
    issueTypeData = issueTypeData.filter((item, i) => item.stateMachineId !== stateMachineId && item.typeCode === 'sub_task');
  } else {
    issueTypeData = issueTypeData.filter((item) => item.stateMachineId !== stateMachineId).filter((item) => !['feature', 'sub_task'].includes(item.typeCode));
    if (isInProgram) {
      issueTypeData = issueTypeData.filter((item) => item.typeCode !== 'issue_epic');
    }
  }
  if (subIssueVOList.length > 0) {
    issueTypeData = issueTypeData.filter((item) => ['task', 'story'].includes(item.typeCode));
  }
  const typeList = (
    <Menu
      style={{
        background: '#fff',
        boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px var(--divider)',
        borderRadius: '2px',
      }}
      className="issue-sidebar-types"
      onClick={handleChangeType}
    >
      {
        issueTypeData.map((t) => (
          <Menu.Item key={t.id} typeCode={t.typeCode} value={t.id} featureType={t.featureType}>
            <TypeTag
              style={{ margin: 0 }}
              data={t}
              showName
              featureType={t.featureType}
            />
          </Menu.Item>
        ))
      }
    </Menu>
  );

  return (
    <div>
      {disabled ? (
        <div
          style={{
            height: 50,
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            justifyContent: 'center',
          }}
        >
          <TypeTag
            data={currentIssueType}
            featureType={featureType}
          />
        </div>
      ) : (
        <Dropdown
          overlay={typeList}
          trigger={['click']}
          disabled={disabled}
        >
          <div
            className="issue-nav-narrow"
          >
            <TypeTag
              data={currentIssueType}
              featureType={featureVO && featureVO.featureType}
            />
            <Icon
              type="arrow_drop_down"
              style={{ fontSize: 16 }}
            />
          </div>
        </Dropdown>
      )}
    </div>
  );
});
export default IssueType;
