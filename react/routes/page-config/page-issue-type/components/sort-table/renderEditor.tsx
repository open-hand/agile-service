import { Button } from 'choerodon-ui';
import {
  Select, Radio, DatePicker, CheckBox,
} from 'choerodon-ui/pro/lib';
import DaysView from 'choerodon-ui/pro/lib/date-picker/DaysView';
import moment, { Moment } from 'moment';
import classnames from 'classnames';
import React, { forwardRef, useState } from 'react';
import styles from './editor.less';

const DatePickerPage = forwardRef((props, ref) => {
  const [value, setValue] = useState<Moment | undefined>();
  const [defaultTime, setDefaultTime] = useState<any>(null);
  const [visible, setVisible] = useState<boolean>(false);
  function handleChange(v: any) {
    setValue(v);
  }
  return (
    <Select
      ref={ref as any}
      trigger={['click'] as any}
      dropdownMatchSelectWidth={false}
      popupContent={(
        <div
          role="none"
          className={styles.date}
        // onClick={(e) => e.stopPropagation()}
        // onMouseDown={(e) => e.stopPropagation()}
        >
          <div className={styles.date_show}>

            <span className={styles.date_show_text}>{value?.format('YYYY-MM-DD')}</span>
          </div>

          <ul className={styles.date_options}>
            <li className={styles.date_options_item}>当前时间</li>
            <li role="none" className={styles.date_options_item} onClick={() => setVisible((old) => !old)}>自定义时间</li>
          </ul>
          <div className={styles.date_picker}>
            <DaysView
              className={classnames(styles.date_picker, { [styles.date_picker_hidden]: !visible })}
              date={value || moment()}
              format="YYYY-MM-DD"
              mode={'date' as any}
              step={{}}
            />
          </div>

        </div>
      )}
    // {...props}
    />
  );
});
function renderEditor(props?: any) {
  const bn = 0;
  console.log('renderEditor', props);
  return <DatePickerPage {...props} />;
}
export default renderEditor;
