/* eslint-disable react/jsx-no-bind */
import React, { Component } from 'react';
import _ from 'lodash';
import {
  Form, Select, Button,
} from 'choerodon-ui';
import { userApi, issueApi } from '@/api';
import { IsProjectMember } from '@/hooks/useIsProjectMember';
import './Assignee.less';
import UserTag from '../tag/user-tag';

const { Option } = Select;
const FormItem = Form.Item;
let sign = false;

class Assignee extends Component {
  debounceFilterIssues = _.debounce((input) => {
    this.setState({ selectLoading: true });
    userApi.getAllInProject(input).then((res) => {
      this.setState({
        originUsers: res.list.filter((u) => u.enabled),
        selectLoading: false,
      });
    });
  }, 500);

  constructor(props) {
    super(props);
    this.state = {
      selectLoading: false,
      assigneeId: undefined,
      originUsers: [],
    };
    props.modal.handleOk(this.handleClickAssignee);
  }

  componentDidMount() {
    this.loadUser();
  }

  handleClickAssignee = () => new Promise((resolve) => {
    const {
      form, issueId, objectVersionNumber, onOk,
    } = this.props;
    form.validateFields((err, values) => {
      if (!err) {
        const { assigneeId } = values;
        const obj = {
          issueId,
          objectVersionNumber,
          assigneeId: assigneeId || null,
        };
        issueApi.update(obj)
          .then((res) => {
            onOk();
            resolve();
          });
      }
    });
  });

  handleFilterChange(input) {
    if (!sign) {
      this.setState({ selectLoading: true });
      userApi.getAllInProject(input).then((res) => {
        this.setState({
          originUsers: res.list.filter((u) => u.enabled),
          selectLoading: false,
        });
      });
      sign = true;
    } else {
      this.debounceFilterIssues(input);
    }
  }

  handleSelectChange(assigneeId) {
    this.setState({ assigneeId });
  }

  handleClickAssigneeToMe() {
    const { assigneeId } = this.state;
    const { form } = this.props;
    userApi.getSelf().then((res) => {
      if (res.id !== assigneeId) {
        this.setState({
          assigneeId: res.id,
          originUsers: [res],
        });
        form.setFieldsValue({
          assigneeId: res.id,
        });
      }
    });
  }

  /**
   * first come in, if assigneeId is not null, load and set into origin
   */
  loadUser() {
    const { assigneeId } = this.props;
    if (!assigneeId) return;
    userApi.getById(assigneeId).then((res) => {
      this.setState({
        assigneeId: res.list[0].id,
        originUsers: res.list.length ? [res.list[0]] : [],
      });
    });
  }

  render() {
    const {
      form: { getFieldDecorator },
    } = this.props;
    const {
      selectLoading, assigneeId, originUsers,
    } = this.state;

    return (
      <div className="c7n-agile-assignee">
        <Form layout="vertical" style={{ width: 400 }}>
          <FormItem>
            {getFieldDecorator('assigneeId', {
              initialValue: assigneeId,
            })(
              <Select
                label="分配给"
                loading={selectLoading}
                allowClear
                filter
                filterOption={false}
                onChange={this.handleSelectChange.bind(this)}
                onFilterChange={this.handleFilterChange.bind(this)}
                getPopupContainer={() => document.getElementById('agile')}
              >
                {originUsers.map((user) => (
                  <Option key={user.id} value={user.id}>
                    <div className="wrap">
                      <UserTag
                        data={user}
                      />
                    </div>
                  </Option>
                ))}
              </Select>,
            )}
          </FormItem>
        </Form>
        <IsProjectMember>
          {(isProjectMember) => isProjectMember && (
            <Button
              className="btn"
              funcType="raised"
              onClick={this.handleClickAssigneeToMe.bind(this)}
            >
              分配给我
            </Button>
          )}
        </IsProjectMember>

      </div>
    );
  }
}
export default Form.create({})(Assignee);
