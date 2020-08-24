import React, { Component } from 'react';
import { withRouter } from 'react-router-dom';
import { stores } from '@choerodon/boot';
import { Icon } from 'choerodon-ui';
import './Card.less';
import to from '@/utils/to';

const { AppState } = stores;

class Card extends Component {
  handleClick() {
    const { link, sprintId } = this.props;
    to(link, {
      params: {
        sprintId,
        paramUrl: `iterationBoard/${sprintId}`,
      },
    });
  }

  render() {
    const { title, children, link } = this.props;
    return (
      <div className="c7n-sprintDashboard-card">
        <div className="card-wrap">
          <header>
            <h1 className="text-overflow-hidden">
              <span>
                {title}
              </span>
            </h1>
            <span className="center" />
            {
              link ? (
                <span>
                  <Icon
                    type="arrow_forward"
                    style={{ cursor: 'pointer' }}
                    onClick={() => this.handleClick()}
                  />
                </span>
              ) : null
            }
          </header>
          <section style={{ padding: '0 16px 1px' }}>
            {children}
          </section>
        </div>
      </div>
    );
  }
}

export default withRouter(Card);
