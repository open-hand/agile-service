import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { Button } from 'choerodon-ui';
import { injectIntl } from 'react-intl';
import { findIndex, map } from 'lodash';
import { hexToRgba } from '../../../../../common/utils';
import TextEditToggle from '../../../../TextEditToggle';
import SelectFocusLoad from '../../../../SelectFocusLoad';

const { Text, Edit } = TextEditToggle;
class TeamItem extends Component {
  constructor(props) {
    super(props);
    this.state = {
      team: props.team,
    };
  }

  renderText = () => {
    const { team } = this.state;
    const {
      isTemp, id, name, sprints,
    } = team;
    if (isTemp) {
      return null;
    }
    const { TEAMSCOLOR, index } = this.props;
    return (
      <div style={{
        color: `${TEAMSCOLOR[index % 6]}`,
        border: `1px solid ${TEAMSCOLOR[index % 6]}`,
        background: `${hexToRgba(TEAMSCOLOR[index % 6], 0.05)}`,
        marginRight: 2,
        marginBottom: 3,
        padding: '1px 2px',
        display: 'block',
        overflow: 'hidden',
        textOverflow: 'ellipsis',
        whiteSpace: 'noWrap',
      }}
      >
        {name}
      </div>
    );
  }

  renderEdit() {
    const { team } = this.state;
    const { sprints = [], isTemp, id } = team;
    const { piId } = this.props;
    return (
      <div>
        <SelectFocusLoad
          value={team.id}
          label="团队"
          style={{
            width: 120,
          }}
          disabled={!isTemp}
          loadWhenMount
          type="sub_project"
        />
        {!isTemp && (
          <SelectFocusLoad
            value={sprints}
            label="冲刺"
            style={{
              width: 120,
            }}
            requestArgs={{
              teamId: id,
              piId,
            }}
            allowClear
            type="sprint_in_project"
            mode="multiple"
            onChange={(value) => {
              // console.log(value);
            }}
          />
        )}
      </div>
    );
  }


  render() {
    return (
      <TextEditToggle
        onSubmit={(data) => {
          // console.log(data);
        }}
      >
        <Text>
          {this.renderText()}
        </Text>
        <Edit>
          {this.renderEdit()}
        </Edit>
      </TextEditToggle>
    );
  }
}
@inject('AppState')
@observer class FieldTeamAndSprint extends Component {
  constructor(props) {
    super(props);
    this.state = {
      tempTeams: [],
    };
  }

  handleDelete = (team) => {
    const { tempTeams } = this.state;
    const { isTemp, key } = team;
    if (isTemp) {
      tempTeams.splice(findIndex(tempTeams, { key }), 1);
      this.setState({
        tempTeams,
      });
    } else {
      // 请求
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

  render() {
    const { store, TEAMSCOLOR } = this.props;
    const { tempTeams } = this.state;
    const issue = store.getIssue;
    const { teamSprint = [] } = issue;
    const teams = map(teamSprint, 'projectVO').concat(tempTeams);
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            负责团队
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <div style={{ display: 'flex' }}>
            <div>
              {teams.map((team, i) => (
                <div>
                  <TeamItem team={team} index={i} TEAMSCOLOR={TEAMSCOLOR} piId={issue.activePi.id} />
                  <Button icon="delete" onClick={this.handleDelete.bind(this, team)} />
                </div>
              ))}
            </div>
            <Button icon="playlist_add" onClick={this.handleAdd} />
          </div>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldTeamAndSprint));
