import React from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import classNames from 'classnames';
import styles from './index.less';

function renderEllipsisBlockOption(text?: string, block?: React.ReactElement, config?: { maxLength?: number, showBlock?: boolean, tooltip?: boolean }) {
  if (!text) {
    return null;
  }
  const { maxLength, tooltip, showBlock } = config || {};
  const suffix = text && maxLength && String(text).length > maxLength ? '...' : undefined;

  const textBlock = (
    <div className={classNames(styles.option_wrap, { [styles.option_wrap_suffix]: !!maxLength })}>
      <span className={classNames({ [styles.ellipsis]: !maxLength })}>{text?.slice(0, maxLength)}</span>
      {suffix}
      {
        showBlock && (
          <div className={styles.current}>{block}</div>
        )
      }
    </div>
  );

  return tooltip
    ? (
      <Tooltip title={text} placement="topLeft" arrowPointAtCenter style={{ zIndex: 9999 }}>
        {textBlock}
      </Tooltip>
    ) : textBlock;
}
const optionStyles = styles as { popup: string, option: string };
export { optionStyles as styles };
export default renderEllipsisBlockOption;
