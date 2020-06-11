import React, { Component } from 'react';
import { Choerodon } from '@choerodon/boot';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { Select, Tooltip } from 'choerodon-ui';
import { injectIntl } from 'react-intl';
import { featureApi, issueApi, epicApi } from '@/api';
import TextEditToggle from '../../../../TextEditToggle';
import IsInProgramStore from '../../../../../stores/common/program/IsInProgramStore';

const { Option } = Select;
const { Text, Edit } = TextEditToggle;

const filterOption = (input, option) => option.props.name && option.props.name.toLowerCase().indexOf(
  input.toLowerCase(),
) >= 0;

@inject('AppState')
@observer class FieldEpic extends Component {
  constructor(props) {
    super(props);
    this.state = {
      originEpics: [],
      originFeatures: [],
      selectLoading: true,
      newFeatureId: undefined,
    };
  }

  componentDidMount() {
    this.init();
  }


  init = () => {
    epicApi.loadEpicsForSelect().then((res) => {
      this.setState({
        originEpics: res,
        selectLoading: false,
      });
    });
    if (IsInProgramStore.isInProgram) {
      featureApi.getByEpicId().then((data) => {
        const { store } = this.props;
        const issue = store.getIssue;
        const {
          featureId, featureName,
        } = issue;
        this.setState({
          originFeatures: (data.find(item => item.issueId === featureId) || !featureId) ? data : [...data, { issueId: featureId, summary: featureName }],
          selectLoading: false,
        });
      });
    }
  };

  updateIssueEpic = async (newEpicId, done) => {
    // const { newEpicId } = this.state;
    const { store, onUpdate, reloadIssue } = this.props;
    const issue = store.getIssue;
    const {
      epicId, issueId, objectVersionNumber, typeCode,
    } = issue;
    if (epicId !== newEpicId) {
      if (typeCode === 'feature' && newEpicId) {
        const hasSame = await featureApi.hasSameInEpicById(issueId, newEpicId);
        if (hasSame) {
          Choerodon.prompt('史诗下已含有同名特性');
          done();
          return;
        }
      }
      const obj = {
        issueId,
        objectVersionNumber,
        epicId: newEpicId || 0,
      };
      issueApi.update(obj)
        .then(() => {
          if (IsInProgramStore.isInProgram) {
            featureApi.getByEpicId().then((data) => {
              this.setState({
                originFeatures: data,
              });
            });
          }
          if (onUpdate) {
            onUpdate();
          }
          if (reloadIssue) {
            reloadIssue(issueId);
          }
        });
    }
  };

  updateIssueFeature = () => {
    const { newFeatureId } = this.state;
    const { store, onUpdate, reloadIssue } = this.props;
    const issue = store.getIssue;
    const { featureId = 1, issueId, objectVersionNumber } = issue;
    if (featureId !== newFeatureId) {
      const obj = {
        issueId,
        objectVersionNumber,
        featureId: newFeatureId || 0,
      };
      issueApi.update(obj)
        .then(() => {
          if (IsInProgramStore.isInProgram) {
            featureApi.getByEpicId().then((data) => {
              this.setState({
                originFeatures: data,
              });
            });
          }
          if (onUpdate) {
            onUpdate();
          }
          if (reloadIssue) {
            reloadIssue(issueId);
          }
        });
    }
  };

  render() {
    const {
      selectLoading, originEpics, originFeatures,
    } = this.state;
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const {
      epicColor, epicId, issueEpicName, typeCode,
      featureId, featureName,
    } = issue;
    return (
      <React.Fragment>
        {typeCode === 'story' && IsInProgramStore.isShowFeature
          ? (
            <div className="line-start mt-10">
              <div className="c7n-property-wrapper">
                <span className="c7n-property">
                  特性
                </span>
              </div>
              <div className="c7n-value-wrapper">
                <TextEditToggle
                  disabled={disabled}
                  formKey="feature"
                  onSubmit={this.updateIssueFeature}
                  originData={featureId || []}
                >
                  <Text>
                    {featureName ? (
                      <div
                        className="primary"
                        style={{ wordBreak: 'break-word' }}
                      >
                        {featureName}
                      </div>
                    ) : (
                      <div>
                        无
                      </div>
                    )
                    }
                  </Text>
                  <Edit>
                    <Select
                      getPopupContainer={() => document.getElementById('detail')}
                      allowClear
                      loading={selectLoading}
                      onChange={(value) => {
                        this.setState({
                          newFeatureId: value,
                        });
                      }}
                      filter
                      filterOption={filterOption}
                      dropdownClassName="c7n-agile-featureField-SelectDropDown"
                    >
                      {originFeatures.map(feature => <Option name={feature.summary} key={`${feature.issueId}`} value={feature.issueId}><Tooltip title={feature.summary}>{feature.summary}</Tooltip></Option>)}
                    </Select>
                  </Edit>
                </TextEditToggle>
              </div>
            </div>
          ) : ''
        }

        <div className="line-start mt-10">
          <div className="c7n-property-wrapper">
            <span className="c7n-property">
              史诗
            </span>
          </div>
          <div className="c7n-value-wrapper">
            <TextEditToggle
              disabled={featureId || disabled}
              formKey="epic"
              onSubmit={this.updateIssueEpic}
              originData={epicId || []}
            >
              <Text>
                {
                  epicId ? (
                    <div
                      style={{
                        color: epicColor,
                        borderWidth: '1px',
                        borderStyle: 'solid',
                        borderColor: epicColor,
                        borderRadius: '2px',
                        fontSize: '13px',
                        lineHeight: '20px',
                        padding: '0 8px',
                        display: 'inline-block',
                        overflow: 'hidden',
                        textOverflow: 'ellipsis',
                      }}
                    >
                      {issueEpicName}
                    </div>
                  ) : (
                    <div>
                      无
                    </div>
                  )
                }
              </Text>
              <Edit>
                <Select
                  getPopupContainer={() => document.getElementById('detail')}
                  allowClear
                  loading={selectLoading}
                  filter
                  filterOption={filterOption}
                  dropdownClassName="c7n-agile-epicField-SelectDropDown"
                >
                  {originEpics.map(epic => <Option name={epic.epicName} key={`${epic.issueId}`} value={epic.issueId}><Tooltip title={epic.epicName}>{epic.epicName}</Tooltip></Option>)}
                </Select>
              </Edit>
            </TextEditToggle>
          </div>
        </div>
      </React.Fragment>
    );
  }
}

export default withRouter(injectIntl(FieldEpic));
