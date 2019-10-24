import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { withRouter, Link } from 'react-router-dom';
import {
  TabPage as Page, Header, Content, stores, Permission, axios, Breadcrumb, Choerodon
} from '@choerodon/boot';
import {
  Table, Breadcrumb as Bread,
} from 'choerodon-ui';
import Edit from './components/edit';
import TableDropMenu from '../../../../common/TableDropMenu';
import './List.less';

const { AppState } = stores;
const { Item } = Bread;
const {
  type, id, name, organizationId: orgId,
} = AppState.currentMenuType;
@observer
class List extends Component {
  constructor(props) {
    super(props);
    this.state = {
      loading: true,
      users: [[], [], []],
      dataSource: [],
      editVisible: false,
      edit: null,
    };
  }

  componentDidMount() {
    this.refresh();
  }

  refresh = () => {
    axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/notice`)
      .then((res) => {
        const createType = [...(res.filter(item => item.event === 'issue_created' && item.enable === true).map(o => o.noticeName))];
        const distributionType = [...(res.filter(item => item.event === 'issue_assigneed' && item.enable === true).map(o => o.noticeName))];
        const solvedType = [...(res.filter(item => item.event === 'issue_solved' && item.enable === true).map(o => o.noticeName))];
        const createUser = res.filter(item => item.event === 'issue_created' && item.noticeName === '用户')[0];
        const distributionUser = res.filter(item => item.event === 'issue_assigneed' && item.noticeName === '用户')[0];
        const solvedUser = res.filter(item => item.event === 'issue_solved' && item.noticeName === '用户')[0];
        this.setState({
          loading: false,
          users: [
            createUser && createUser.user && createUser.user !== 'null' ? createUser.idWithNameVOList : [],
            distributionUser && distributionUser.user && distributionUser.user !== 'null' ? distributionUser.idWithNameVOList : [],
            solvedUser && solvedUser.user && solvedUser.user !== 'null' ? solvedUser.idWithNameVOList : [],
          ],
          dataSource: [{
            key: 'issue_created',
            event: '问题创建',
            notificationType: createType,
          }, {
            key: 'issue_assigneed',
            event: '问题分配',
            notificationType: distributionType,
          }, {
            key: 'issue_solved',
            event: '问题已解决',
            notificationType: solvedType,
          }],
        });
      })
      .catch(() => {
        this.setState({
          loading: false,
        });
        Choerodon.prompt('获取信息失败');
      });
  }

  handleEditClick = (key) => {
    this.setState({
      editVisible: true,
      edit: key,
    });
  }

  handleEditCancel = () => {
    this.setState({
      editVisible: false,
      edit: null,
    });
  }

  handleEditOk = () => {
    this.refresh();
    this.setState({
      editVisible: false,
      edit: null,
    });
  }

  getColumn() {
    const { users } = this.state;
    const columns = [
      {
        title: '事件',
        dataIndex: 'event',
        key: 'event',
        width: '30%',
        render: (text, record) => (
          <TableDropMenu
            text={text}
            permission
            type={type}
            projectId={id}
            organizationId={orgId}
            service={['agile-service.notice.updateNotice']}
            onClickEdit={this.handleEditClick.bind(this, record.key)}
          />
        ),
      },
      {
        title: '通知对象',
        dataIndex: 'notificationType',
        key: 'notificationType',
        width: '62%',
        render: (text, record, index) => (
          (text && text.length > 0 ? (
            <ul className="notificationTypeList">
              {
                text.slice(0, 20).map((item) => {
                  if (item !== '用户') {
                    if (item === '当前处理人') {
                      return <li>经办人</li>;
                    }
                    return (
                      <li>{item}</li>
                    );
                  } else if (item === '用户') {
                    return (
                      <li>
                        {`用户: ${users && users.length && users[index].length > 0 ? users[index].map(o => o.name).join(', ') : '-'}`}
                      </li>
                    );
                  }
                  return null;
                })
              }
            </ul>
          ) : '-')
        )
        ,
      },
    ];
    return columns;
  }

  render() {
    const {
      loading, dataSource, editVisible, edit,
    } = this.state;
    return (
      <Permission type={type} projectId={id} organizationId={orgId} service={['agile-service.notice.queryByProjectId']}>
        <Page className="c7n-editFieldConfiguration">
          <Breadcrumb />
          <Content>
            <Table
              dataSource={dataSource}
              columns={this.getColumn()}
              rowKey={record => record.key}
              pagination={false}
              filterBarPlaceholder="过滤表"
              loading={loading}
            />
            {editVisible && <Edit edit={edit} onCancel={this.handleEditCancel} onEditOk={this.handleEditOk} />}
          </Content>
        </Page>
      </Permission>
    );
  }
}

export default withRouter(List);
