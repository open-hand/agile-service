import React, { Component } from 'react';
import {
  Input, Menu, Dropdown, Icon,
} from 'choerodon-ui';
import { Choerodon } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import { checkCanQuickCreate } from '@/utils/quickCreate';
import { issueApi, fieldApi } from '@/api';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import { TypeTag } from '@/components';
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

    if (e?.path.some((i) => i.id === 'create-stroy-issue-type-menu') || !adding) {
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
      const req = {
        epicId: epic.issueId,
        featureId: feature.issueId === 'none' ? 0 : feature.issueId,
        projectId: getProjectId(),
        summary: value,
        typeCode: 'story',
        issueTypeId: currentTypeId,
        priorityCode: `priority-${defaultPriority.id}`,
        priorityId: defaultPriority.id,
        ...swimLine === 'version' && version.versionId !== 'none' ? {
          versionIssueRelVOList: [{
            ...version,
            relationType: 'fix',
          }],
        } : {},
        ...swimLine === 'sprint' && sprint.sprintId !== 'none' ? {
          sprintId: sprint.sprintId,
        } : {},
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
      issueApi.create(req).then((res) => {
        const dto = {
          schemeCode: 'agile_issue',
          issueTypeId: res.issueTypeId,
          pageCode: 'agile_issue_create',
        };
        this.setState({
          adding: false,
          value: '',
        });
        const { versionIssueRelVOList } = res;
        onCreate({ ...res, storyMapVersionDTOList: versionIssueRelVOList, storyMapSprintList: [{ sprintId: sprint && sprint.sprintId !== 'none' ? sprint.sprintId : 0 }] });
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

  handleChangeType=({ key }) => {
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
          boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px rgba(0, 0, 0, 0.12)',
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
          display: 'flex',
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
                <Input autoFocus onPressEnter={this.handleCreateIssue} placeholder="在此创建新内容" value={value} onChange={this.handleChange} maxLength={44} />
              </div>
            )
            : (
              <div className="c7nagile-StoryMap-CreateStory-btn">
                <span role="none" className="primary" style={{ cursor: 'pointer' }} onClick={this.handleAddStoryClick}>新建问题</span>
                {' '}
                或
                {' '}
                <span role="none" className="primary" style={{ cursor: 'pointer' }} onClick={this.handleSourceClick}>从需求池引入</span>
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
  const { data: issueTypes } = useProjectIssueTypes({ onlyEnabled: true });
  return (
    <ClickOutSideCreateStory
      issueTypes={(issueTypes || []).filter(({ typeCode }) => typeCode === 'story')}
      {...props}
    />
  );
};
export default WrapCreateStory;
