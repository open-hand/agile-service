package io.choerodon.agile.infra.utils;

public final class SagaTopic {

    private SagaTopic() {
    }

    public static class Project {
        private Project() {
        }

        /**
         * 创建项目SagaCode
         */
        public static final String PROJECT_CREATE = "iam-create-project";

        /**
         * 更新项目SagaCode
         */
        public static final String PROJECT_UPDATE = "iam-update-project";
        /**
         * 创建项目SagaTaskCode
         */
        public static final String TASK_PROJECT_CREATE = "agile-create-project";

        /**
         * 更新项目SagaTaskCode
         */
        public static final String TASK_PROJECT_UPDATE = "agile-update-project";

        /**
         * devops删除tag
         */
        public static final String GIT_TAG_DELETE = "devops-git-tag-delete";

        public static final String TASK_GIT_TAG_DELETE = "agile-git-tag-delete";

    }

    public static class Organization {
        private Organization() {
        }

        /**
         * 创建组织SagaCode
         */
        public static final String ORG_CREATE = "org-create-organization";

        /**
         * 创建项目SagaTaskCode
         */
        public static final String TASK_ORG_CREATE = "agile-create-organization";

    }


}
