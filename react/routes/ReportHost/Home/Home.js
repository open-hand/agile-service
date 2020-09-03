import React, { Component } from 'react';
import {
  Page, Header, Content, stores,
} from '@choerodon/boot';
import './style';
import to from '@/utils/to';
import list from './list';

const { AppState } = stores;

class Home extends Component {
  handleClickItem(report) {
    to(report.link);
  }

  renderContentLinks() {
    return list.map((report) => (
      <div
        key={report.key}
        className="c7n-item"
        role="none"
        onClick={this.handleClickItem.bind(this, report)}
      >
        <div className="c7n-item-pic">
          <div className={`c7n-item-picWrap ${report.pic}`} />
        </div>
        <div className="c7n-item-word">
          <h4 className="c7n-item-title">{report.title}</h4>
          <p className="c7n-item-des">{report.des}</p>
        </div>
      </div>
    ));
  }

  render() {
    return (
      <Page>
        <Header title="敏捷报表" />
        <Content>
          <div className="c7n-reportHome-pane">
            {this.renderContentLinks()}
          </div>
        </Content>
      </Page>
    );
  }
}

export default Home;
