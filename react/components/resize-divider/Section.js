import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Styles from './Section.less';

class Section extends Component {
  render() {
    const { type, size, style } = this.props;
    const { width, height } = size;
    return (
      <div {...this.props} style={{ ...style, width: width || 'auto', height: height || 'auto' }} className={`${Styles.Section} ${Styles[type]}`} />
    );
  }
}

Section.propTypes = {
  // bind: PropTypes.string.isRequired,
};

export default Section;
