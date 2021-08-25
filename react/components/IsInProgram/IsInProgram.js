import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { commonApi } from '@/api';
import { OldLoading as Loading } from '../Loading';

class IsInProgram extends Component {
  // eslint-disable-next-line react/state-in-constructor
  state = {
    loading: false,
    program: null,
  }

  componentDidMount() {
    this.setState({
      loading: true,
    });
    commonApi.getProjectsInProgram().then((res) => {
      this.setState({
        program: res,
        loading: false,
      });
    });
  }

  renderChildren=() => {
    const { program } = this.state;
    const { children, project: ProjectElement } = this.props;
    if (program) {
      return children(program);
    }
    return ProjectElement || null;
  }

  render() {
    const { loading } = this.state;
    return (loading ? <Loading /> : this.renderChildren());
  }
}

IsInProgram.propTypes = {

};

export default IsInProgram;
