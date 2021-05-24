import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { CellPadding } from '../../Constants';

class Cell extends Component {
  render() {
    const {
      children, style, saveRef, lastCollapse, collapse, epicIndex, ...otherProps
    } = this.props;
    return (
      <td
        {...otherProps}
        ref={saveRef}
        style={{
          padding: CellPadding,
          boxShadow: epicIndex === 0 ? 'inset 0 -1px 0 var(--divider)' : 'inset 1px -1px 0 var(--divider)',
          // border: 'none',
          // ...isLastEpic ? { borderRight: 'solid 1px var(--divider)' } : {},
          ...lastCollapse ? { boxShadow: 'inset 0 -1px 0 var(--divider)' } : {},
          ...collapse ? { boxShadow: 'none' } : {},
          ...style,
        }}
      >
        {children}
      </td>
    );
  }
}

Cell.propTypes = {

};
Cell.defaultProps = {

};
export default Cell;
