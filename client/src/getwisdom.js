require("../static/vendor/wisdom.js");

const setup = ({ userId, isAdmin, username }) => {
  wisdom("init", 844);
  wisdom("identify", userId);
  wisdom("setUserInfo", { traits: { isAdmin, username } });
};

exports.setup = setup;
