import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Button, Icon, Spin,
} from 'choerodon-ui';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import moment from 'moment';
import {
  reportApi, boardApi,
} from '@/api';
import { linkUrl } from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import AccumulationChart from '@/components/charts/accumulation';
import AccumulationSearch from '@/components/charts/accumulation/search';
import './AccumulationHome.less';
import '../../BurndownChart/BurndownChartHome/BurndownChartHome.less';
import NoDataComponent from '../../Component/noData';
import pic from '../../../../assets/image/emptyChart.svg';
import SwithChart from '../../Component/switchChart';

@observer
class AccumulationHome extends Component {
  constructor(props) {
    super(props);
    this.state = {
      data: [],
      loading: true,
      boardId: null,
      quickFilterIds: [],
      range: [moment().subtract(2, 'months'), moment()],
      columnIds: [],
    };
  }

  handleBoardChange = async (boardId) => {
    this.setState({
      boardId,
    });
    const boardData = await boardApi.load(boardId);
    this.setState({
      columnIds: boardData.columnsData.columns.map((column) => column.columnId),
    }, () => {
      this.getData();
    });
  }

  getData() {
    this.setState({
      loading: true,
    });
    const {
      boardId, columnIds, range, quickFilterIds,
    } = this.state;
    const [startDate, endDate] = range;
    reportApi.loadCumulativeData({
      columnIds,
      endDate: `${endDate.format('YYYY-MM-DD')} 23:59:59`,
      quickFilterIds,
      startDate: startDate.format('YYYY-MM-DD 00:00:00'),
      boardId,
    }).then((res) => {
      this.setState({
        data: res,
        loading: false,
      });
    }).catch((error) => {
      this.setState({
        loading: false,
      });
    });
  }

  renderContent() {
    const { loading, data } = this.state;
    if (loading) {
      return (
        <div style={{ display: 'flex', justifyContent: 'center', marginTop: '100px' }}>
          <Spin />
        </div>
      );
    }
    if (!data.length) {
      return (
        <NoDataComponent title="问题" links={[{ name: '问题管理', link: '/agile/work-list/issue' }]} img={pic} />
      );
    }
    return (
      <div className="c7n-accumulation-report" style={{ flexGrow: 1, height: '100%' }}>
        <AccumulationChart data={data} loading={loading} />
      </div>
    );
  }

  render() {
    const {
      quickFilterIds, range, boardId,
    } = this.state;
    return (
      <Page service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.cumulative_flow_diagram']}>
        <Header
          title="累积流量图"
          backPath={linkUrl(LINK_URL.report)}
        >
          <SwithChart
            current="accumulation"
          />
          <Button funcType="flat" onClick={() => { this.getData(); }}>
            <Icon type="refresh icon" />
            <span>刷新</span>
          </Button>
        </Header>
        <Breadcrumb title="累积流量图" />
        <Content
          style={{
            display: 'flex',
            flexDirection: 'column',
          }}
        >
          <div className="c7n-accumulation-filter">
            <AccumulationSearch
              range={range}
              onRangeChange={(value) => {
                this.setState({
                  range: value,
                }, () => {
                  this.getData();
                });
              }}
              boardId={boardId}
              onBoardChange={this.handleBoardChange}
              quickFilterIds={quickFilterIds}
              onQuickSearchChange={(value) => {
                this.setState({
                  quickFilterIds: value,
                }, () => {
                  this.getData();
                });
              }}
            />
          </div>
          {this.renderContent()}
        </Content>
      </Page>
    );
  }
}

export default AccumulationHome;
