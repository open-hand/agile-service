/* eslint-disable import/no-anonymous-default-export */
/* eslint-disable camelcase */
import React from 'react';
import { Select } from 'choerodon-ui';
import { find, castArray } from 'lodash';
import { Tooltip } from 'choerodon-ui/pro';
import {
  userApi, componentApi, issueApi, epicApi, versionApi,
  issueTypeApi, commonApi, issueLabelApi, priorityApi, statusApi,
  featureApi, piApi, sprintApi, fieldApi,
} from '@/api';
import { issueLinkTypeApi } from '@/api/IssueLinkType';
import TypeTag from '../TypeTag';
import StatusTag from '../StatusTag';
import UserTag from '../tag/user-tag';

// 增加 typeof 避免选项中 加载更多 影响
const filterOption = (input, option) => option.props.children && typeof (option.props.children) === 'string' && option.props.children.toLowerCase().indexOf(
  input.toLowerCase(),
) >= 0;
const filterOptionByName = (input, option) => option.props.name && typeof (option.props.name) === 'string' && option.props.name.toLowerCase().indexOf(
  input.toLowerCase(),
) >= 0;
export function transform(links) {
  // split active and passive
  const active = links.map((link) => ({
    name: link.outWard,
    isIn: false,
    linkTypeId: link.linkTypeId,
  }));
  const passive = [];
  links.forEach((link) => {
    if (link.inWard !== link.outWard) {
      passive.push({
        name: link.inWard,
        isIn: true,
        linkTypeId: link.linkTypeId,
      });
    }
  });

  return active.concat(passive);
}
const { Option, OptGroup } = Select;
const issue_type_program = {
  props: {
    filterOption,
  },
  request: () => new Promise((resolve) => issueTypeApi.loadAllWithStateMachineId('program').then((issueTypes) => {
    resolve(issueTypes);
  })),
  render: (issueType) => (
    <Option
      key={issueType.id}
      value={issueType.id}
      name={issueType.name}
    >
      <div style={{ display: 'inline-flex', alignItems: 'center', padding: '2px' }}>
        <TypeTag
          data={issueType}
          showName
        />
      </div>
    </Option>
  ),
};
export default {
  user: {
    request: ({ filter, page }) => userApi.getAllInProject(filter, page).then((UserData) => ({ ...UserData, list: UserData.list.filter((user) => user.enabled) })),
    render: (user) => (
      <Option key={user.id} value={user.id}>
        <div style={{
          display: 'inline-flex', alignItems: 'center', padding: 2, verticalAlign: 'sub',
        }}
        >
          <UserTag
            data={user}
          />
        </div>
      </Option>
    ),
    avoidShowError: (props, List) => new Promise((resolve) => {
      const { value } = props;
      const extraList = [];
      const values = value instanceof Array ? value : [value];
      const requestQue = [];
      values.forEach((a) => {
        if (a && typeof a === 'string' && a !== '0' && !find(List, { id: a })) {
          requestQue.push(userApi.getById(a));
        }
      });
      Promise.all(requestQue).then((users) => {
        users.forEach((res) => {
          if (res.list && res.list.length > 0) {
            extraList.push(res.list[0]);
          }
        });
        resolve(extraList);
      }).catch(() => {
        resolve(extraList);
      });
    }),
  },
  issue_status: {
    props: {
      filterOption,
    },
    request: () => statusApi.loadByProject('agile'),
    render: (status) => (
      <Option
        key={status.id}
        value={status.id}
        name={status.name}
      >
        {status.name}
      </Option>
    ),
  },
  status_program: {
    request: () => new Promise((resolve) => statusApi.loadByProject('program').then((statusList) => {
      resolve(statusList);
    })),
    render: (status) => (
      <Option
        key={status.id}
        value={status.id}
        name={status.name}
      >
        <div style={{ display: 'inline-flex', alignItems: 'center', padding: '2px' }}>
          <StatusTag
            data={status}
          />
        </div>
      </Option>
    ),
  },
  epic: {
    props: {
      filterOption,
    },
    request: () => epicApi.loadEpics(),
    render: (epic) => (
      <Option
        key={epic.issueId}
        value={epic.issueId}
      >
        {epic.epicName}
      </Option>
    ),
  },
  epic_program: {
    props: {

      filterOption:
        (input, option) => option.props.children
          && option.props.children.toLowerCase().indexOf(
            input.toLowerCase(),
          ) >= 0,
    },
    request: epicApi.loadProgramEpics,
    render: (epic) => (
      <Option
        key={epic.issueId}
        value={epic.issueId}
      >
        {epic.epicName}
      </Option>
    ),
  },
  issue_type_program,
  issue_type_program_simple: {
    ...issue_type_program,
    render: (issueType) => (
      <Option
        key={issueType.id}
        value={issueType.id}
        name={issueType.name}
      >
        {issueType.name}
      </Option>
    ),
  },
  issue_type: {
    props: {
      filterOption,
    },
    request: () => issueTypeApi.loadAllWithStateMachineId('agile'),
    render: (issueType) => (
      <Option
        key={issueType.id}
        value={issueType.id}
        name={issueType.name}
      >
        {issueType.name}
      </Option>
    ),
  },
  issue_type_program_feature_epic: {
    ...issue_type_program,
    request: () => new Promise((resolve) => issueTypeApi.loadAllWithStateMachineId('program').then((issueTypes) => {
      const featureTypes = [{
        id: 'business',
        name: '特性',
      }, {
        id: 'enabler',
        name: '使能',
      }];
      const epicType = find(issueTypes, { typeCode: 'issue_epic' });
      resolve([...featureTypes, epicType]);
    })),
    render: (issueType) => (
      <Option
        key={issueType.id}
        value={issueType.id}
        name={issueType.name}
      >
        {issueType.name}
      </Option>
    ),
  },
  custom_field: {
    props: {

      loadWhenMount: true,
    },
    request: ({ filter, page }, { fieldId, selected, projectId } = {}) => fieldApi.project(projectId).getFieldOptions(fieldId, filter, page, 10, castArray(selected).filter(Boolean)),
    render: (option) => (
      <Option
        key={option.id}
        value={option.id}
      >
        <Tooltip title={option.value} placement="top" arrowPointAtCenter>
          <span>{option.value}</span>
        </Tooltip>
      </Option>
    ),
  },
  issue_link: {
    props: {

      filter: false,
      filterOption: false,
      loadWhenMount: true,
    },
    request: () => issueLinkTypeApi.getAll().then((res) => transform(res.list)),
    render: (link) => (
      <Option value={`${link.linkTypeId}+${link.isIn}`}>
        {link.name}
      </Option>
    ),
  },
  issues_in_link: {
    props: {
      mode: 'multiple',
      optionLabelProp: 'showName',

    },
    request: ({ filter, page }, { issueId, excludeIssueIds, projectId } = {}) => issueApi.project(projectId).loadIssuesInLink(page, 20, issueId, filter, excludeIssueIds),
    render: (issue) => (
      <Option
        key={issue.issueId}
        value={issue.issueId}
        showName={issue.issueNum}
      >
        <div style={{
          display: 'inline-flex',
          flex: 1,
          width: 'calc(100% - 30px)',
          alignItems: 'center',
          verticalAlign: 'middle',
        }}
        >
          <TypeTag
            data={issue.issueTypeVO}
          />
          <span style={{
            paddingLeft: 12, paddingRight: 12, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
          }}
          >
            {issue.issueNum}
          </span>
          <div style={{ overflow: 'hidden', flex: 1 }}>
            <Tooltip title={issue.summary}>
              <p style={{
                paddingRight: '25px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0, maxWidth: 'unset',
              }}
              >
                {issue.summary}
              </p>
            </Tooltip>

          </div>
        </div>
      </Option>
    ),
  },
  subTask_parent_issue: {
    props: {
      optionLabelProp: 'showName',
      loadWhenMount: true,

    },
    request: ({ filter, page }, requestArgs) => issueApi.project(requestArgs?.projectId).loadParentIssues(page, 20, 'sub_task', filter), // 故事、任务、缺陷（不能是子缺陷）
    render: (issue) => (
      <Option
        key={issue.issueId}
        value={issue.issueId}
        showName={`${issue.issueNum} ${issue.summary}`}
      >
        <div style={{
          display: 'inline-flex',
          flex: 1,
          width: 'calc(100% - 30px)',
          alignItems: 'center',
          verticalAlign: 'middle',
        }}
        >
          <TypeTag
            data={issue.issueTypeVO}
          />
          <span style={{
            paddingLeft: 12, paddingRight: 12, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
          }}
          >
            {issue.issueNum}
          </span>
          <div style={{ overflow: 'hidden', flex: 1 }}>
            <Tooltip title={issue.summary}>
              <p style={{
                paddingRight: '25px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0, maxWidth: 'unset',
              }}
              >
                {issue.summary}
              </p>
            </Tooltip>
          </div>
        </div>
      </Option>
    ),
  },
  subBug_parent_issue: {
    props: {
      optionLabelProp: 'showName',
      loadWhenMount: true,

    },
    request: ({ filter, page }, requestArgs) => issueApi.project(requestArgs?.projectId).loadParentIssues(page, 20, 'bug', filter), // 故事、任务
    render: (issue) => (
      <Option
        key={issue.issueId}
        value={issue.issueId}
        showName={`${issue.issueNum} ${issue.summary}`}
      >
        <div style={{
          display: 'inline-flex',
          flex: 1,
          width: 'calc(100% - 30px)',
          alignItems: 'center',
          verticalAlign: 'middle',
        }}
        >
          <TypeTag
            data={issue.issueTypeVO}
          />
          <span style={{
            paddingLeft: 12, paddingRight: 12, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
          }}
          >
            {issue.issueNum}
          </span>
          <div style={{ overflow: 'hidden', flex: 1 }}>
            <Tooltip title={issue.summary}>
              <p style={{
                paddingRight: '25px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0, maxWidth: 'unset',
              }}
              >
                {issue.summary}
              </p>
            </Tooltip>
          </div>
        </div>
      </Option>
    ),
  },
  features_in_link: {
    props: {
      mode: 'multiple',
      optionLabelProp: 'showName',

    },
    request: ({ filter, page }, issueId) => featureApi.loadFeaturesInLink(page, 20, issueId, filter),
    render: (issue) => (
      <Option
        key={issue.featureId}
        value={issue.featureId}
        showName={issue.issueNum}
      >
        <div style={{
          display: 'inline-flex',
          flex: 1,
          width: 'calc(100% - 30px)',
          alignItems: 'center',
          verticalAlign: 'middle',
        }}
        >
          <TypeTag
            data={issue.issueTypeVO}
          />
          <span style={{
            paddingLeft: 12, paddingRight: 12, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
          }}
          >
            {issue.issueNum}
          </span>
          <div style={{ overflow: 'hidden', flex: 1 }}>
            <Tooltip title={issue.summary}>
              <p style={{
                paddingRight: '25px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0, maxWidth: 'unset',
              }}
              >
                {issue.summary}
              </p>
            </Tooltip>
          </div>
        </div>
      </Option>
    ),
  },
  priority: {
    props: {

      filter: false,
      filterOption: false,
      loadWhenMount: true,
    },
    request: () => priorityApi.loadByProject(),
    getDefaultValue: (priorities) => find(priorities, { default: true }).id,
    render: (priority) => (
      <Option key={priority.id} value={priority.id} label={priority.name}>
        <div style={{ display: 'inline-flex', alignItems: 'center', padding: 2 }}>
          <span>{priority.name}</span>
        </div>
      </Option>
    ),
  },
  component: {
    props: {

      // filterOption: filterOptionByName,
      // onFilterChange: false,
      loadWhenMount: true,
    },
    request: ({ filter, page }) => componentApi.loadAllComponents(filter, undefined, page, 10),
    render: (component) => (
      <Option
        key={component.name}
        value={component.name}
        name={component.name}
      >
        <Tooltip title={component.name} placement="top" arrowPointAtCenter>
          <span>{component.name}</span>
        </Tooltip>
      </Option>
    ),
  },
  label: {
    props: {

      filter: false,
      filterOption: false,
      loadWhenMount: true,
    },
    request: () => issueLabelApi.loads(),
    render: (label) => (
      <Option key={label.labelName} value={label.labelName}>
        {label.labelName}
      </Option>
    ),
  },
  label_id: {
    props: {

      filter: true,
      filterOption,
      loadWhenMount: true,
    },
    request: () => issueLabelApi.loads(),
    render: (label) => (
      <Option key={label.labelId} value={label.labelId}>
        {label.labelName}
      </Option>
    ),
  },
  version: {
    props: {

      filterOption,
      onFilterChange: false,
      loadWhenMount: true,
    },
    request: ({ filter, page }, statusList = ['version_planning']) => versionApi.loadNamesByStatus(statusList),
    render: (version) => (
      <Option
        key={version.versionId}
        value={version.versionId}
      >
        {version.name}
      </Option>
    ),
  },
  sprint: {
    props: {

      filterOption,
      loadWhenMount: true,
    },
    request: ({ filter, page }, statusList = ['sprint_planning', 'started']) => sprintApi.loadSprints(statusList),
    render: (sprint) => (
      <Option key={sprint.sprintId} value={sprint.sprintId}>
        {sprint.sprintName}
      </Option>
    ),
  },
  sprint_in_project: {
    props: {

      filterOption,
      loadWhenMount: true,
    },
    request: ({ filter, page }, { teamId, piId }) => sprintApi.loadSprintsByTeam(teamId, piId),
    render: (sprint) => (
      <Option key={sprint.sprintId} value={sprint.sprintId}>
        {sprint.sprintName}
      </Option>
    ),
  },
  pi: {
    props: {

      filterOption,
      loadWhenMount: true,
    },
    request: () => piApi.getUnfinished(),
    render: (pi, disabledCurrentPI) => (
      <Option disabled={disabledCurrentPI && pi.statusCode === 'doing'} key={pi.id} value={pi.id}>
        {pi.fullName || `${pi.code}-${pi.name}`}
      </Option>
    ),
  },
  feature: {
    request: ({ filter, page }, requestArgs) => featureApi.getByEpicId(undefined, filter, page),
    render: (item) => (
      <Option key={`${item.issueId}`} value={item.issueId}>{item.summary}</Option>
    ),
    props: {

      loadWhenMount: true,
    },
    avoidShowError: (props, List) => new Promise((resolve) => {
      const { selectedFeature } = props;
      const extraList = [];
      const values = selectedFeature instanceof Array ? selectedFeature : [selectedFeature];
      values.forEach((feature) => {
        if (feature && !find(List, { issueId: feature.issueId })) {
          extraList.push(feature);
        }
      });
      resolve(extraList);
    }),
  }, // 特性列表
  feature_all: {
    request: ({ filter, page }, requestArgs) => featureApi.queryAllInSubProject(requestArgs, filter, page),
    render: (item) => (
      <Option key={`${item.issueId}`} value={item.issueId}>
        <Tooltip title={item.summary}>
          {item.summary}
        </Tooltip>
      </Option>
    ),
    props: {

      // filterOption,
      loadWhenMount: true,
    },
  }, // 特性列表
  sub_project: {
    props: {

      filterOption: filterOptionByName,
      onFilterChange: false,
      loadWhenMount: true,
    },
    request: () => commonApi.getSubProjects(true),
    render: (pro) => (
      <Option key={pro.projectId} value={pro.projectId} name={pro.projName}>
        <Tooltip title={pro.projName}>{pro.projName}</Tooltip>
      </Option>
    ),
  },
  sub_sprint: {
    props: {

      filterOption,
      onFilterChange: false,
      loadWhenMount: true,
    },
    request: ({ filter, page }, { piId, teamIds }) => sprintApi.getTeamSprints(piId, teamIds),
    render: (team) => (
      <OptGroup label={team.projectVO.name} key={team.projectVO.id}>
        {(team.sprints || []).map((sprint) => (
          <Option key={`${sprint.sprintId}`} value={sprint.sprintId}>
            <Tooltip placement="topRight" title={sprint.sprintName}>{sprint.sprintName}</Tooltip>
          </Option>
        ))}
      </OptGroup>
    ),
  },

};
