import React, { useContext, useMemo } from 'react';
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
import Styles from './IssueType.less';
import useIsProgram from '@/hooks/useIsProgram';
import { WATERFALL_TYPE_CODES } from '../../../constants/TYPE_CODE';

const IssueType = observer(({
  reloadIssue, onTransformType,
}) => {
  const {
    store, disabled, menuType, isProgramIssue,
  } = useContext(EditIssueContext);
  const { isShowFeature } = useIsInProgram({ projectId: store.projectId });
  const { isAgileProgram } = useIsProgram();
  const issue = store.getIssue;
  const {
    issueTypeVO = {}, featureVO = {}, subIssueVOList = [], applyType,
  } = issue;
  const { typeCode, id: issueTypeId } = issueTypeVO;
  const queryTypeCodes = useMemo(() => {
    if (subIssueVOList.length > 0) {
      return ['task', 'story'];
    }
    if (typeCode === 'sub_task') {
      return ['sub_task'];
    }
    let codes = ['story', 'bug', 'task', 'issue_epic'];
    if (isShowFeature || isAgileProgram || menuType === 'org') {
      codes = ['story', 'bug', 'task'];
    }
    if (WATERFALL_TYPE_CODES.includes(typeCode)) {
      codes = WATERFALL_TYPE_CODES;
    }

    return codes;
  }, [isAgileProgram, isShowFeature, menuType, subIssueVOList.length, typeCode]);
  let { data: issueTypeData } = useProjectIssueTypes({
    onlyEnabled: true, typeCode: queryTypeCodes, applyType, projectId: store.projectId,
  }, { enabled: !disabled });
  const handleChangeType = async (type) => {
    const {
      issueId, objectVersionNumber, summary,
    } = issue;
    const { featureType, value } = type.item.props;
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
  const { stateMachineId } = find(issueTypeData, { id: issueTypeId }) || {};
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
  } else if (isProgramIssue && typeCode === 'issue_epic') {
    issueTypeData = [];
  } else {
    issueTypeData = issueTypeData.filter((item, i) => item.stateMachineId !== stateMachineId);
  }
  const typeList = (
    <Menu
      style={{
        background: '#fff',
        boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px var(--divider)',
        borderRadius: '2px',
      }}
      className={Styles.sidebarTypeMenu}
      onClick={handleChangeType}
    >
      {
        issueTypeData.map((t) => (
          <Menu.Item key={t.id} typeCode={t.typeCode} value={t.id} featureType={t.featureType}>
            <TypeTag
              data={t}
              showName
              featureType={t.featureType}
              className={Styles.dropdownMenuTag}
            />
          </Menu.Item>
        ))
      }
    </Menu>
  );

  return (
    <div>
      {disabled ? (
        <div className={Styles.issueTypeWrap}>
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
          <div className={Styles.issueNavArrow}>
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
