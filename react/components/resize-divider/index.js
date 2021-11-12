import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Divider from './Divider';
import Section from './Section';
import Styles from './index.less';

class Index extends Component {
  saveRef = (name) => (ref) => {
    this[name] = ref;
  };

  renderChildren = () => {
    const children = React.Children.toArray(this.props.children);
    const newChildren = children.map((child, i) => {
      if (child.type.displayName === 'Divider') {
        return React.cloneElement(
          child,
          {
            ...child.props,
            parent: this,
            bindings: [`Section_${i - 1}`, `Section_${i + 1}`],
            type: this.props.type,
          },
        );
      }

      return React.cloneElement(
        child,
        {
          ...child.props,
          ref: this.saveRef(`Section_${i}`),
          type: this.props.type,
        },
      );
    });
    return newChildren;
  };

  render() {
    const { type, style } = this.props;

    return (
      <div className={`${Styles.container} ${Styles[type]}`} style={style}>
        {this.renderChildren()}
      </div>
    );
  }
}

Index.defaultProps = {
  type: 'horizontal',
};
Index.propTypes = {
  type: PropTypes.oneOf(['vertical', 'horizontal']),
};
Index.Divider = Divider;
Index.Section = Section;

export default Index;
