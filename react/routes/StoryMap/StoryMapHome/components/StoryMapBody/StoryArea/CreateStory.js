import React, { Component } from 'react';
import {
  Input, Menu, Dropdown, Icon, Tooltip,
} from 'choerodon-ui';
import { Choerodon } from '@choerodon/boot';
import { isEmpty } from 'lodash';
import { getProjectId } from '@/utils/common';
import { checkCanQuickCreate } from '@/utils/quickCreate';
import { issueApi, fieldApi } from '@/api';
import { TypeTag } from '@/components';
import { fields2Map } from '@/utils/defaultValue';
import { useStoryMapContext } from '@/routes/StoryMap/StoryMapHome';
import Card from '../Card';
import './CreateStory.less';
import StoryMapStore from '../../../../../../stores/project/StoryMap/StoryMapStore';
import clickOutSide from '../../../../../../components/CommonComponent/ClickOutSide';

class CreateStory extends Component {
  // 防止重复创建
  canAdd = true;

  constructor(props) {
    super(props);
    this.state = {
      adding: false,
      value: '',
      currentTypeId: props.issueTypes[0]?.id,

    };
  }

  static getDerivedStateFromProps(nextProps, prevState) {
    if (!prevState.currentTypeId && nextProps.issueTypes.length > 0) {
      return {
        currentTypeId: nextProps.issueTypes[0]?.id,
      };
    }
    return null;
  }

  handleClickOutside = (e) => {
    const { adding } = this.state;

    if (e.composedPath()?.some((i) => i.id === 'create-stroy-issue-type-menu') || !adding) {
      return;
    }
    this.handleCreateIssue();
  };

  handleCreateIssue = async () => {
    if (!this.canAdd) {
      return;
    }
    this.canAdd = false;
    const { value, currentTypeId } = this.state;
    if (value && value.trim()) {
      const { swimLine } = StoryMapStore;
      const {
        onCreate, epic, feature, version, sprint,
      } = this.props;
      const storyType = StoryMapStore.getIssueTypeByCode('story');
      const defaultPriority = StoryMapStore.getDefaultPriority;
      const param = {
        schemeCode: 'agile_issue',
        issueTypeId: currentTypeId,
        pageCode: 'agile_issue_create',
      };
      const propsVersionIssueRelVOList = swimLine === 'version' && version.versionId !== 'none' ? [{
        ...version,
        relationType: 'fix',
      }] : undefined;
      const sprintId = swimLine === 'sprint' && sprint.sprintId !== 'none' ? sprint.sprintId : undefined;
      const fields = await fieldApi.getFields(param);
      const fieldsMap = fields2Map(fields);
      const versionIssueRelVOList = propsVersionIssueRelVOList || [];
      const defaultVersionList = [];
      if (!isEmpty(fieldsMap.get('influenceVersion')?.defaultValue) && !versionIssueRelVOList.some((item = {}) => item.relationType === 'influence')) {
        fieldsMap.get('influenceVersion')?.defaultValue.forEach((item) => defaultVersionList.push({
          versionId: item,
          relationType: 'influence',
        }));
      }
      if (!isEmpty(fieldsMap.get('fixVersion')?.defaultValue) && !versionIssueRelVOList.some((item = {}) => item.relationType === 'fix')) {
        fieldsMap.get('fixVersion')?.defaultValue.forEach((item) => defaultVersionList.push({
          versionId: item,
          relationType: 'fix',
        }));
      }
      versionIssueRelVOList.push(...defaultVersionList);
      const issue = {
        priorityCode: `priority-${defaultPriority.id}`,
        priorityId: defaultPriority.id,
        projectId: getProjectId(),
        programId: getProjectId(),
        epicId: epic.issueId || 0,
        summary: value.trim(),
        issueTypeId: currentTypeId,
        typeCode: 'story',
        parentIssueId: 0,
        relateIssueId: 0,
        featureVO: {},
        sprintId: sprintId || fieldsMap.get('sprint')?.defaultValue || 0,
        epicName: currentTypeId === 'issue_epic' ? value.trim() : undefined,
        componentIssueRelVOList: fieldsMap.get('component')?.defaultValueObjs || [],
        description: '',
        issueLinkCreateVOList: [],
        labelIssueRelVOList: fieldsMap.get('label')?.defaultValueObjs || [],
        versionIssueRelVOList,
        fixVersionIssueRel: fieldsMap.get('fixVersion')?.defaultValue || [],
        featureId: feature.issueId === 'none' ? 0 : feature.issueId,
        assigneeId: fieldsMap.get('assignee')?.defaultValue,
        reporterId: fieldsMap.get('reporter')?.defaultValue,
        estimatedEndTime: fieldsMap.get('estimatedEndTime')?.defaultValue,
        estimatedStartTime: fieldsMap.get('estimatedStartTime')?.defaultValue,
        storyPoints: fieldsMap.get('storyPoints')?.defaultValue,
        remainingTime: fieldsMap.get('remainingTime')?.defaultValue,
        mainResponsibleId: fieldsMap.get('mainResponsible')?.defaultValue,
        testResponsibleId: fieldsMap.get('testResponsible')?.defaultValue,
      };

      if (!await checkCanQuickCreate(storyType.id)) {
        Choerodon.prompt('该问题类型含有必填选项，请使用创建问题弹框创建');
        this.canAdd = true;
        this.setState({
          adding: false,
          value: '',
        });
        return;
      }
      issueApi.create(issue).then((res) => {
        const dto = {
          schemeCode: 'agile_issue',
          issueTypeId: res.issueTypeId,
          pageCode: 'agile_issue_create',
        };
        this.setState({
          adding: false,
          value: '',
        });
        const { versionIssueRelVOList: storyMapVersionDTOList } = res;
        onCreate({ ...res, storyMapVersionDTOList, storyMapSprintList: [{ sprintId: sprintId || fieldsMap.get('sprint')?.defaultValue || 0 }] });
        fieldApi.quickCreateDefault(res.issueId, dto);
      }).finally(() => {
        this.canAdd = true;
      });
    } else {
      this.canAdd = true;
      this.setState({
        adding: false,
        value: '',
      });
    }
  }

  handleAddStoryClick = () => {
    this.setState({
      adding: true,
    });
  }

  handleChange = (e) => {
    this.setState({
      value: e.target.value,
    });
  }

  handleSourceClick = () => {
    StoryMapStore.setSideIssueListVisible(true);
  }

  handleChangeType = ({ key }) => {
    this.setState({
      currentTypeId: key,
    });
  }

  render() {
    const { adding, value, currentTypeId } = this.state;
    const { issueTypes } = this.props;
    const currentType = issueTypes.find((t) => t.id === currentTypeId);

    const typeList = (
      <Menu
        style={{
          background: '#fff',
          boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px var(--divider)',
          borderRadius: '2px',
        }}
        id="create-stroy-issue-type-menu"
        onClick={this.handleChangeType}
      >
        {
          issueTypes.map((type) => (
            <Menu.Item key={type.id}>
              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                <TypeTag
                  data={type}
                  showName
                />
              </div>
            </Menu.Item>
          ))
        }
      </Menu>
    );
    return (
      <Card
        style={{
          boxShadow: adding ? '0 0 4px -2px rgba(0,0,0,0.50), 0 2px 4px 0 rgba(0,0,0,0.13)' : '',
          borderRadius: 2,
          padding: 7,

          justifyContent: 'center',
        }}
        className="c7nagile-StoryMap-CreateStory"
      >
        {
          adding
            ? (
              <div style={{ display: 'inline-flex', position: 'relative' }}>
                <Dropdown overlay={typeList} trigger={['click']}>
                  <div style={{
                    display: 'flex',
                    alignItems: 'center',
                    position: 'absolute',
                    top: 4,
                    zIndex: 3,
                  }}
                  >
                    <TypeTag
                      data={currentType}
                      iconSize={20}
                    />
                    <Icon
                      type="arrow_drop_down"
                      style={{ fontSize: 16, marginLeft: -7 }}
                    />
                  </div>
                </Dropdown>
                <Input border={false} autoFocus onPressEnter={this.handleCreateIssue} placeholder="作为什么角色，我想要什么样的结果，以便于怎样的目的" value={value} onChange={this.handleChange} maxLength={44} />
              </div>
            )
            : (
              <div className="c7nagile-StoryMap-CreateStory-btn">
                {issueTypes.length > 0 ? <span role="none" className="primary" style={{ cursor: 'pointer' }} onClick={this.handleAddStoryClick}>新建问题</span>
                  : <Tooltip title="问题类型中所有故事类型被禁用，无法新建问题"><span role="none" style={{ color: 'rgba(0, 0, 0, 0.26)' }}>新建问题</span></Tooltip>}

                {' '}
                或
                {' '}
                <span role="none" className="primary" style={{ cursor: 'pointer' }} onClick={this.handleSourceClick}>从未规划列表引入</span>
              </div>
            )
        }
      </Card>
    );
  }
}

CreateStory.propTypes = {

};
const ClickOutSideCreateStory = clickOutSide(CreateStory);
const WrapCreateStory = (props) => {
  const { issueTypes } = useStoryMapContext();
  return (
    <ClickOutSideCreateStory
      issueTypes={issueTypes}
      {...props}
    />
  );
};
export default WrapCreateStory;
