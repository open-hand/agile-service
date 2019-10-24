import React, { Component } from 'react';
import classnames from 'classnames';
import './StatusTag.less';
import { STATUS } from '../../common/Constant';

class StatusTag extends Component {
  // shouldComponentUpdate(nextProps, nextState) {
  //   if (
  //     nextProps.name === this.props.name
  //     && nextProps.color === this.props.color
  //   ) {
  //     return false;
  //   }
  //   return true;
  // }

  renderStatusBackground = (categoryCode) => {
    switch (categoryCode) {
      case 'todo':
        return 'rgb(255, 177, 0)';
      case 'doing':
        return 'rgb(77, 144, 254)';
      case 'done':
        return 'rgb(0, 191, 165)';
      case 'prepare':
        return '#F67F5A';
      default:
        return 'gray';
    }
  };

  render() {
    const {
      name,
      color,
      data,
      style,
      categoryCode,
      inTable,
    } = this.props;
    return (
      <div
        className={classnames('c7n-statusTag', {
          'c7nagile-statusTag-table': inTable,
        })}
        style={{
          background: color || (categoryCode && this.renderStatusBackground(categoryCode)) || (data && STATUS[data.type]) || 'transparent',
          lineHeight: inTable ? '16px' : '20px',
          height: inTable ? '16px' : '20px',
          ...style,
        }}
      >
        <div>{name || (data && data.name) || ''}</div>
      </div>
    );
  }
}
export default StatusTag;
