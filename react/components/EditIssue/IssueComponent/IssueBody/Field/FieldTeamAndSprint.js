import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { Button, Popconfirm } from 'choerodon-ui';
import { injectIntl } from 'react-intl';
import { find, findIndex } from 'lodash';
import { updateFeatureTeamAndSprint, removeFeatureTeam } from '@/api/FeatureApi';
import TextEditToggle from '../../../../TextEditToggle';
import SelectFocusLoad from '../../../../SelectFocusLoad';

const { Text, Edit } = TextEditToggle;
function SprintTag({ children }) {
  return (
    <div
      style={{
        color: 'rgb(74, 77, 97)',
        border: '1px solid rgb(74, 77, 97)',
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
      teamId: props.team.id,
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
    const { team } = this.props;
    return (
      <div>{`${team.name}${team.sprints.length > 0 ? '：' : ''}`}</div>
    );
  }

  renderSprintText = () => {
    const { team } = this.props;
    const { sprints } = team;
    return sprints.length > 0 && (
      <div>
        {sprints.map(sprint => (
          <SprintTag>
            {sprint.sprintName}
          </SprintTag>
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
      <div style={{ display: 'flex', flexWrap: 'wrap' }}>
        {this.renderTeamText()}
        {this.renderSprintText()}
      </div>
    );
  }

  renderEdit() {
    const { teamId } = this.state;
    const { team } = this.props;
    const { isTemp } = team;
    const { sprintIds } = this.state;
    const { piId, optionFilter } = this.props;
    return (
      <Fragment>
        {isTemp ? (
          <SelectFocusLoad
            value={teamId}
            label="团队"
            style={{
              width: '100%',
              minWidth: 150,
            }}
            disabled={!isTemp}
            loadWhenMount
            optionFilter={optionFilter}
            type="sub_project"
            onChange={(value) => {
              this.setState({
                teamId: value,
              });
            }}
          />
        ) : this.renderTeamText()}
        {!isTemp && (
          <SelectFocusLoad
            value={sprintIds}
            label="冲刺"
            style={{
              width: '100%',
              minWidth: 150,
              marginTop: 5,
            }}
            requestArgs={{
              teamId,
              piId: piId || 0,
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
      </Fragment>
    );
  }

  handleSubmit = async () => {
    const { teamId, sprintIds } = this.state;
    const { onSubmit, team } = this.props;
    onSubmit({ ...team, id: teamId }, sprintIds);
  }

  render() {
    const { team } = this.props;
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
            : team.isTemp
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
    const { store, disabled } = this.props;
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
          <div style={{ display: 'flex', width: '100%', flexWrap: 'wrap' }}>
            {teams.length > 0 && (
              <div>
                {teams.map(team => (
                  <TeamItem
                    key={team.id || team.key}
                    team={team}
                    piId={issue.activePi ? issue.activePi.id : undefined}
                    {...this.props}
                    onSubmit={this.handleSubmit}
                    onDelete={this.handleDelete}
                    // 已经选择的团队，过滤掉
                    optionFilter={pro => pro.projectId === team.teamProjectId || !find(teams, { teamProjectId: pro.projectId })}
                  />
                ))}
              </div>
            )}
            {!disabled && <Button icon="add" onClick={this.handleAdd}>添加团队</Button>}
          </div>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldTeamAndSprint));
