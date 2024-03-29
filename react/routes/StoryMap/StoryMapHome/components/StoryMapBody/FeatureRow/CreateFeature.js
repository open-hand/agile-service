import React, { Component } from 'react';
import PropTypes from 'prop-types';
import {
  Input, Dropdown, Icon, Menu,
} from 'choerodon-ui';
import { find } from 'lodash';
import { getProjectId } from '@/utils/common';
import { issueApi, fieldApi } from '@/api';
import TypeTag from '../../../../../../components/TypeTag';
import Card from '../Card';
import clickOutSide from '../../../../../../components/CommonComponent/ClickOutSide';
import StoryMapStore from '../../../../../../stores/project/StoryMap/StoryMapStore';

class CreateFeature extends Component {
  // 防止重复创建
  canAdd = true;

  state = {
    featureType: 'business',
    value: '',
  }

  handleClickOutside = (e) => {
    this.handleCreateIssue();
  };

  handleCreateIssue = () => {
    if (!this.canAdd) {
      return;
    }
    this.canAdd = false;
    const { value } = this.state;
    if (value !== '') {
      const { onCreate, epicId } = this.props;
      const { featureType } = this.state;
      const featureTypeVO = StoryMapStore.getFeatureType;
      const defaultPriority = StoryMapStore.getDefaultPriority;
      const req = {
        projectId: getProjectId(),
        epicId,
        summary: value,
        typeCode: 'feature',
        featureVO: {
          featureType,
        },
        issueTypeId: featureTypeVO.id,
        priorityCode: `priority-${defaultPriority.id}`,
        priorityId: defaultPriority.id,
      };
      issueApi.create(req).then((res) => {
        const dto = {
          schemeCode: 'agile_issue',
          issueTypeId: res.issueTypeId,
          pageCode: 'agile_issue_create',
        };
        onCreate({ ...res, featureType });
        fieldApi.quickCreateDefault(res.issueId, dto);
      }).finally(() => {
        this.canAdd = true;
      });
    } else {
      this.canAdd = true;
      const { epicId } = this.props;
      StoryMapStore.removeAddingFeature(epicId);
    }
  }

  handleChange=(e) => {
    this.setState({
      value: e.target.value,
    });
  }

  handleChangeType=(type) => {
    this.setState({
      featureType: type.item.props.value,
    });
  }

  render() {
    const { featureType: currentType, value } = this.state;
    const featureType = find(StoryMapStore.issueTypes, { typeCode: 'feature' });
    const types = [
      {
        ...featureType,
        colour: '#3D5AFE',
        featureType: 'business',
        name: '特性',
        id: 'business',
      }, {
        ...featureType,
        colour: '#FFCA28',
        featureType: 'enabler',
        name: '使能',
        id: 'enabler',
      },
    ];
    const typeList = (
      <Menu
        style={{
          background: '#fff',
          boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px var(--divider)',
          borderRadius: '2px',
        }}
        className="issue-sidebar-types"
        onClick={this.handleChangeType}
      >
        {
          types.map((t) => (
            <Menu.Item value={t.featureType}>
              <TypeTag
                style={{ margin: 0 }}
                data={t}
                showName
              />
            </Menu.Item>
          ))
        }
      </Menu>
    );
    return (
      <Card style={{
        boxShadow: '0 0 4px -2px rgba(0,0,0,0.50), 0 2px 4px 0 rgba(0,0,0,0.13)',
        borderRadius: 2,
        margin: '4px 4px 4px 9px',
        padding: 7,
      }}
      >
        <Input autoFocus value={value} placeholder="在此创建新内容" onChange={this.handleChange} onPressEnter={this.handleCreateIssue} maxLength={44} />
        <Dropdown overlay={typeList} trigger={['click']} >
          <div style={{ display: 'flex', marginTop: 5 }}>
            <TypeTag
              data={find(types, { featureType: currentType })}
              featureType={currentType}
              showName
            />
            <Icon
              type="arrow_drop_down"
              style={{ fontSize: 16, marginLeft: 5 }}
            />
          </div>
        </Dropdown>
      </Card>
    );
  }
}

CreateFeature.propTypes = {

};

export default clickOutSide(CreateFeature);
