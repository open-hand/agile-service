import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Input } from 'choerodon-ui';
import { Choerodon } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import { issueApi, fieldApi } from '@/api';
import { checkCanQuickCreate } from '@/utils/quickCreate';
import openCreateIssue from '@/components/create-issue';
import Card from './Card';
import StoryMapStore from '../../../../../stores/project/StoryMap/StoryMapStore';
import clickOutSide from '../../../../../components/CommonComponent/ClickOutSide';

class CreateEpic extends Component {
  // 防止重复创建
  canAdd = true;

  state = {
    value: '',
  }

  handleClickOutside = () => {
    this.handleCreateIssue();
  };

  handleCreateIssue = async () => {
    if (!this.canAdd) {
      return;
    }
    this.canAdd = false;
    // console.log(e.target.value);
    const { value } = this.state;
    if (value !== '' && value.trim()) {
      const { onCreate, index } = this.props;
      const epicType = StoryMapStore.getEpicType;
      const defaultPriority = StoryMapStore.getDefaultPriority;
      const preEpic = StoryMapStore.getEpicList[index - 1];
      const rankVO = {
        projectId: getProjectId(),
        // objectVersionNumber: source.epicRankObjectVersionNumber, // 乐观锁
        // issueId: source.issueId,
        type: 'epic',
        before: false, // 是否拖动到第一个
        referenceIssueId: preEpic ? preEpic.issueId : 0,
      };
      const req = {
        projectId: getProjectId(),
        epicName: value,
        summary: value,
        typeCode: 'issue_epic',
        issueTypeId: epicType.id,
        priorityCode: `priority-${defaultPriority.id}`,
        priorityId: defaultPriority.id,
        rankVO,
      };
      if (!await checkCanQuickCreate(epicType.id)) {
        if (!openCreateIssue) {
          Choerodon.prompt('该工作项类型含有必填选项，请使用创建工作项弹框创建');
          this.canAdd = true;
          StoryMapStore.removeAddingEpic();
          return;
        }
        Choerodon.prompt('请填写标注的必填字段');
        openCreateIssue({
          defaultValues: {
            summary: value,
            epicName: value,
          },
          defaultTypeId: epicType.id,
          onCreate: (res) => {
            this.canAdd = true;
            onCreate({ ...res, epicName: value });
          },
          extraSubmitValue: {
            rankVO,
          },
        });
        return;
      }
      issueApi.create(req).then((res) => {
        if (res.failed) {
          if (res.code === 'error.epicName.exist') {
            Choerodon.prompt('史诗名称已存在');
          } else {
            Choerodon.prompt('创建失败');
          }
          return;
        }
        const dto = {
          schemeCode: 'agile_issue',
          issueTypeId: res.issueTypeId,
          pageCode: 'agile_issue_create',
        };
        onCreate({ ...res, epicName: value });
        fieldApi.quickCreateDefault(res.issueId, dto);
      }).finally(() => {
        this.canAdd = true;
      });
    } else {
      this.canAdd = true;
      StoryMapStore.removeAddingEpic();
    }
  }

  handleChange = (e) => {
    this.setState({
      value: e.target.value,
    });
  }

  render() {
    const { value } = this.state;
    return (
      <Card style={{
        boxShadow: '0 0 4px -2px rgba(0,0,0,0.50), 0 2px 4px 0 rgba(0,0,0,0.13)',
        borderRadius: 2,
        height: 64,
        margin: '4px 4px 4px 9px',
        padding: 7,
        display: 'flex',
        justifyContent: 'center',
      }}
      >
        <Input autoFocus onPressEnter={this.handleCreateIssue} placeholder="史诗是开发或需求的核心内容，例如：用户管理" maxLength="22" value={value} onChange={this.handleChange} />
      </Card>
    );
  }
}

CreateEpic.propTypes = {

};

export default clickOutSide(CreateEpic);
