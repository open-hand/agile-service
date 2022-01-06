import React, { ReactNode } from 'react';
import UIDateTimesView from 'choerodon-ui/pro/lib/date-picker/DateTimesView';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import moment from 'moment';

export default class DateTimesView extends UIDateTimesView {
  static displayName = 'DateTimesView';

  static type = FieldType.dateTime;

  renderFooter(): ReactNode {
    const {
      prefixCls,
      props: { date },
    } = this;
    return (
      <div className={`${prefixCls}-footer`}>
        <a className={`${prefixCls}-footer-now-btn`} onClick={this.choose.bind(this, moment(), false)}>
          此刻
        </a>
        <a className={`${prefixCls}-footer-view-select`} onClick={this.handleTimeSelect}>
          {date.format('HH:mm')}
        </a>
      </div>
    );
  }
}
