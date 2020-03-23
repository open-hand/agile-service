import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { Button, Popconfirm } from 'choerodon-ui';
import { injectIntl } from 'react-intl';
import { find, findIndex } from 'lodash';
import { updateFeatureTeamAndSprint, removeFeatureTeam } from '@/api/FeatureApi';
import { hexToRgba } from '../../../../../common/utils';
import TextEditToggle from '../../../../TextEditToggle';
import SelectFocusLoad from '../../../../SelectFocusLoad';

const { Text, Edit } = TextEditToggle;
function getColor(TEAMSCOLOR, index) {
  return {
    color: `${TEAMSCOLOR[index % 6]}`,
    border: `1px solid ${TEAMSCOLOR[index % 6]}`,
    background: `${hexToRgba(TEAMSCOLOR[index % 6], 0.05)}`,
  };
}
function TeamTag({ TEAMSCOLOR, index, children }) {
  return (
    <div
      style={{
        ...getColor(TEAMSCOLOR, index),
        height: 25,
        marginRight: 2,
        marginBottom: 3,
        padding: '1px 2px',
        display: 'block',
        overflow: 'hidden',
        textOverflow: 'ellipsis',
        whiteSpace: 'noWrap',
      }}
    >
      {children}
    </div>
  );
}
@observer
class TeamItem extends Component {
  constructor(props) {
    super(props);
    this.state = {
      team: props.team,
      sprintIds: props.team.sprints.map(sprint => sprint.sprintId),
    };
    this.ref = React.createRef();
  }

  componentDidMount() {
    const { team } = this.props;
    // 添加后默认编辑状态
    if (team.isTemp) {
      this.ref.current.enterEditing();
    }
  }

  renderTeamText = () => {
    const { TEAMSCOLOR, index, team } = this.props;
    return (
      <TeamTag TEAMSCOLOR={TEAMSCOLOR} index={index}>{team.name}</TeamTag>
    );
  }

  renderSprintText = () => {
    const { TEAMSCOLOR, team } = this.props;
    const { sprints } = team;
    return sprints.length > 0 && (
      <div>
        {sprints.map((sprint, i) => (
          <TeamTag TEAMSCOLOR={TEAMSCOLOR} index={i}>
            {sprint.sprintName}
          </TeamTag>
        ))}
      </div>
    );
  }

  renderText = () => {
    const { team } = this.props;
    const { isTemp } = team;
    if (isTemp) {
      return null;
    }
    return (
      <div style={{ display: 'flex', alignItems: 'flex-start' }}>
        {this.renderTeamText()}
        {this.renderSprintText()}
      </div>

    );
  }

  renderEdit() {
    const { team } = this.props;
    const { isTemp, id } = team;
    const { sprintIds } = this.state;
    const { piId, optionFilter } = this.props;
    return (
      <div style={{ display: 'flex', alignItems: 'center' }}>
        {isTemp ? (
          <SelectFocusLoad
            value={team.id}
            label="团队"
            style={{
              flex: 1,
              maxWidth: 200,
            }}
            disabled={!isTemp}
            loadWhenMount
            optionFilter={optionFilter}
            type="sub_project"
            onChange={(value) => {
              this.setState({
                team: { ...team, id: value },
              });
            }}
          />
        ) : this.renderTeamText()}
        {!isTemp && (
          <SelectFocusLoad
            value={sprintIds}
            label="冲刺"
            style={{
              flex: 1,
            }}
            requestArgs={{
              teamId: id,
              piId,
            }}
            allowClear
            type="sprint_in_project"
            mode="multiple"
            onChange={(value) => {
              this.setState({
                sprintIds: value,
              });
            }}
          />
        )}
      </div>
    );
  }

  handleSubmit = async () => {
    const { team, sprintIds } = this.state;
    const { onSubmit } = this.props;
    onSubmit(team, sprintIds);
  }

  render() {
    const { team } = this.state;
    const { disabled } = this.props;
    return (
      <div style={{ display: 'flex' }}>
        <TextEditToggle
          disabled={disabled}
          onSubmit={this.handleSubmit}
          style={{ maxWidth: 'unset' }}
          saveRef={this.ref}
        >
          <Text>
            {this.renderText()}
          </Text>
          <Edit>
            {this.renderEdit()}
          </Edit>
        </TextEditToggle>

        {// eslint-disable-next-line no-nested-ternary
          disabled
            ? null
            : !team.isTemp
              ? <Button icon="delete" onClick={() => { this.props.onDelete(team); }} shape="circle" style={{ flexShrink: 0 }} />
              : (
                <Popconfirm disabled={team.isTemp} title="确认删除关联团队吗" onConfirm={() => { this.props.onDelete(team); }}>
                  <Button icon="delete" shape="circle" style={{ flexShrink: 0 }} />
                </Popconfirm>
              )}
      </div>
    );
  }
}

@observer
class FieldTeamAndSprint extends Component {
  constructor(props) {
    super(props);
    this.state = {
      tempTeams: [],
    };
  }

  handleDelete = async (team) => {
    const { tempTeams } = this.state;
    const { store, reloadIssue } = this.props;
    const issue = store.getIssue;
    const { isTemp, key } = team;
    if (isTemp) {
      tempTeams.splice(findIndex(tempTeams, { key }), 1);
      this.setState({
        tempTeams,
      });
    } else {
      // 请求
      await removeFeatureTeam({
        teamId: team.id,
        featureId: issue.issueId,
        piId: issue.activePi ? issue.activePi.id : null,
      });
      reloadIssue();
    }
  }

  handleAdd = () => {
    const { tempTeams } = this.state;
    tempTeams.push({
      key: Math.random(),
      isTemp: true,
      sprints: [],
    });
    this.setState({
      tempTeams,
    });
  }

  handleSubmit = async (team, sprintIds) => {
    const { store, reloadIssue } = this.props;
    const issue = store.getIssue;
    if (team.isTemp && !team.id) {
      this.handleDelete(team);
    }
    if (team.isTemp && team.id) {
      // 增加团队
      await updateFeatureTeamAndSprint({
        teamProjectId: team.id,
        featureId: issue.issueId,
        piId: issue.activePi ? issue.activePi.id : null,
      });
      this.setState({
        tempTeams: [],
      });
      reloadIssue();
    }
    if (!team.isTemp) {
      // 判断冲刺有没有变化
      const addSprints = sprintIds.filter(sprintId => !find(team.sprints, { sprintId }));
      const removeSprints = team.sprints.filter(sprint => sprint.sprintId && !sprintIds.includes(sprint.sprintId)).map(sprint => sprint.sprintId);
      if (addSprints.length > 0 || removeSprints.length > 0) {
        // 请求设置冲刺
        await updateFeatureTeamAndSprint({
          teamProjectId: team.id,
          featureId: issue.issueId,
          piId: issue.activePi ? issue.activePi.id : null,
          sprintIds: addSprints,
          deleteSprintIds: removeSprints,
        });
        reloadIssue();
      }
    }
  }

  render() {
    const { store, TEAMSCOLOR, disabled } = this.props;
    const { tempTeams } = this.state;
    const issue = store.getIssue;
    const { teamSprint } = issue;
    const teams = (teamSprint || []).map(team => ({ ...team, ...team.projectVO })).concat(tempTeams);
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            负责团队
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <div style={{ display: 'flex', width: '100%' }}>
            <div style={{ flex: 1, maxWidth: 250 }}>
              {teams.map((team, i) => (
                <TeamItem
                  key={team.id || team.key}
                  team={team}
                  index={i}
                  TEAMSCOLOR={TEAMSCOLOR}
                  piId={issue.activePi.id}
                  {...this.props}
                  onSubmit={this.handleSubmit}
                  onDelete={this.handleDelete}
                  // 已经选择的团队，过滤掉
                  optionFilter={pro => pro.projectId === team.teamProjectId || !find(teams, { teamProjectId: pro.projectId })}
                />
              ))}
            </div>
            {!disabled && <Button icon="playlist_add" onClick={this.handleAdd} shape="circle" />}
          </div>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldTeamAndSprint));
