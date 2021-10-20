import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import { featureApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectTeam from '@/components/select/select-team';

@observer
class FieldTeam extends Component {
  handleSubmit = async (teamIds = []) => {
    const {
      store, onUpdate, reloadIssue,
    } = this.props;
    const issue = store.getIssue;
    const { issueId } = issue;
    const activePiTeams = issue.activePiTeams || [];
    const originTeamIds = activePiTeams.map((team) => String(team.id));
    const addTeams = teamIds ? teamIds.filter((teamId) => !originTeamIds.includes(teamId)) : [];
    const removeTeams = teamIds ? originTeamIds.filter((teamId) => !teamIds.includes(teamId)) : originTeamIds;
    await featureApi.updateTeamAndSprint({
      piId: issue.activePi ? issue.activePi.id : null,
      deleteSprintIds: [],
      featureId: issue.issueId,
      sprintIds: [],
      teamProjectIds: addTeams,
      deleteTeamProjectIds: removeTeams,
    });
    if (onUpdate) {
      onUpdate();
    }
    await reloadIssue(issueId);
  }

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const activePiTeams = issue.activePiTeams || [];
    const teamIds = activePiTeams.map((team) => String(team.id));
    const field = store.getFieldByCode('sprint');
    const required = field?.required || store.getRuleRequired(field);

    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            负责的子项目
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled}
            onSubmit={this.handleSubmit}
            initValue={teamIds}
            editor={(
              <SelectTeam
                multiple
                projectId={store.projectId}
                required={required}
                label="团队"
                style={{
                  width: '100%',
                  minWidth: 150,
                }}
                {
                  ...store.getOptionsData(field, teamIds)
                }
              />
            )}
          >
            {activePiTeams.length > 0 ? activePiTeams.map((team) => team.name).join(' 、 ') : '无'}
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldTeam));
